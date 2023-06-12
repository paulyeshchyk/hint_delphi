unit OPP.Help.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  OPP.Help.Settings.Value.Editor,
  OPP.Help.System.Codable.TunningEditorDefaultSettings,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxListView,
  Vcl.StdActns;

type

  TOPPHelpSettingsForm = class(TForm)
    actionClose: TAction;
    actionEditValue: TAction;
    ActionList1: TActionList;
    actionSave: TAction;
    Button1: TButton;
    cxListView1: TcxListView;
    Panel1: TPanel;
    procedure actionCloseExecute(Sender: TObject);
    procedure actionEditValueExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fEditorDefaults: TOPPHelpHintTunningEditorDefaultSettings;
    procedure SaveEditorDefaults();
    procedure OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
    procedure SetSettingEditorDefaults(const AValue: TOPPHelpHintTunningEditorDefaultSettings);
  public
    { Public declarations }
    property EditorDefaultSettings: TOPPHelpHintTunningEditorDefaultSettings read fEditorDefaults write SetSettingEditorDefaults;
  end;

  TOPPHelpSettingsFormHelper = class helper for TOPPHelpSettingsForm
    class function GetEditorDefaults(): TOPPHelpHintTunningEditorDefaultSettings;
    class procedure SetEditorDefaults(ADefaults: TOPPHelpHintTunningEditorDefaultSettings);
  end;

var
  OPPHelpSettingsForm: TOPPHelpSettingsForm;

implementation

uses
  OPP.Help.predicate, OPP.Help.System.Types,
  OPP.Help.log,
  OPP.Help.System.error,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.Helper,

  CommCtrl;

const
  kContext = 'OPPSettingsForm';
  kShortcutFilePathPropertyName = 'ShortcutFilePath';
  kHintsFilePathPropertyName = 'HintsFilePath';

const
  SSettingsFileName = 'TSampleForm.Default.settings';

resourcestring
  SEventSettingsWasSaved = 'settings was saved';
  SHintFilePathCaption = 'Путь к файлу подсказок';
  SShortFilePathCaption = 'Путь к файлу помощи';

{$R *.dfm}

type
  TOPPCxListView = class helper for TcxListView
  public
    function GetSettingsValue: TOPPHelpSettingsValue;
    procedure LoadEditorDefaults(ASettings: TOPPHelpHintTunningEditorDefaultSettings);
  end;

class function TOPPHelpSettingsFormHelper.GetEditorDefaults: TOPPHelpHintTunningEditorDefaultSettings;
begin
  try
    TOPPCodableHelper<TOPPHelpHintTunningEditorDefaultSettings>.Decode(SSettingsFileName, result);
  except
    on E: Exception do
    begin
      result := nil;
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPHelpSettingsFormHelper.SetEditorDefaults(ADefaults: TOPPHelpHintTunningEditorDefaultSettings);
begin
  try
    TOPPCodableHelper<TOPPHelpHintTunningEditorDefaultSettings>.Encode(SSettingsFileName, ADefaults);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

procedure TOPPHelpSettingsForm.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPHelpSettingsForm.actionEditValueExecute(Sender: TObject);
var
  Editor: TOPPHelpSettingsValueEditor;
  fValue: TOPPHelpSettingsValue;
begin
  if not assigned(cxListView1.Selected) then
    exit;

  fValue := cxListView1.GetSettingsValue;

  Editor := TOPPHelpSettingsValueEditor.Create(self);
  try
    Editor.setValue(fValue);
    Editor.onCompletion := self.OnEditorCompletion;
    Editor.ShowModal;
  finally
    Editor.Free;
  end;
end;

procedure TOPPHelpSettingsForm.actionSaveExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPHelpSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveEditorDefaults();
  if assigned(EditorDefaultSettings) then
    EditorDefaultSettings.Free;
end;

procedure TOPPHelpSettingsForm.FormCreate(Sender: TObject);
var
  fDefaults: TOPPHelpHintTunningEditorDefaultSettings;
begin
  try
    TOPPCodableHelper<TOPPHelpHintTunningEditorDefaultSettings>.Decode(SSettingsFileName, fDefaults);
    self.EditorDefaultSettings := fDefaults;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPHelpSettingsForm.OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
begin
  if not Saved then
    exit;

  if AValue.propertyName = kHintsFilePathPropertyName then
    fEditorDefaults.HintsFilePath := AValue.propertyValue;
  if AValue.propertyName = kShortcutFilePathPropertyName then
    fEditorDefaults.ShortcutFilePath := AValue.propertyValue;
  cxListView1.LoadEditorDefaults(self.EditorDefaultSettings);
end;

procedure TOPPHelpSettingsForm.SaveEditorDefaults;
begin
  try
    TOPPCodableHelper<TOPPHelpHintTunningEditorDefaultSettings>.Encode(SSettingsFileName, EditorDefaultSettings);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPHelpSettingsForm.SetSettingEditorDefaults(const AValue: TOPPHelpHintTunningEditorDefaultSettings);
begin
  fEditorDefaults := AValue;
  cxListView1.LoadEditorDefaults(AValue);
end;

{ TOPPCxListView }

function TOPPCxListView.GetSettingsValue: TOPPHelpSettingsValue;
begin
  result.propertyCaption := self.Selected.Caption;
  result.propertyValue := self.Selected.Subitems[0];
  result.propertyName := self.Selected.Subitems[1];
end;

procedure TOPPCxListView.LoadEditorDefaults(ASettings: TOPPHelpHintTunningEditorDefaultSettings);
var
  fItem: TListItem;
begin
  self.Clear;
  if not assigned(ASettings) then
    exit;

  fItem := self.Items.Add;
  fItem.Caption := SHintFilePathCaption;
  fItem.Subitems.Add(ASettings.HintsFilePath);
  fItem.Subitems.Add(kHintsFilePathPropertyName);
  //
  fItem := self.Items.Add;
  fItem.Caption := SShortFilePathCaption;
  fItem.Subitems.Add(ASettings.ShortcutFilePath);
  fItem.Subitems.Add(kShortcutFilePathPropertyName);

  self.Columns[0].Width := LVSCW_AUTOSIZE or LVSCW_AUTOSIZE_USEHEADER;
  self.Columns[1].Width := LVSCW_AUTOSIZE or LVSCW_AUTOSIZE_USEHEADER;
end;

end.
