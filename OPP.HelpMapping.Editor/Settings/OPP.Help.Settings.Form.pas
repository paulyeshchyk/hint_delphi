unit OPP.Help.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  OPP.Help.Settings.Value.Editor,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxListView,
  Vcl.StdActns,
  OPP.Help.Defaults, OPP.Help.Settings.Manager;

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
    ffDefaults: TOPPHelpDefaults;
    procedure OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
    procedure saveSettings();
    procedure setDefaults(const AValue: TOPPHelpDefaults);
  public
    class function CreateUserDefaults(): TOPPHelpDefaults;
    class function GetUserDefaults(): TOPPHelpDefaults;
    class procedure SetUserDefaults(ADefaults: TOPPHelpDefaults);
    { Public declarations }
    property Defaults: TOPPHelpDefaults read ffDefaults write setDefaults;
  end;

  TOPPCxListView = class helper for TcxListView
  public
    procedure loadSettings(ASettings: TOPPHelpDefaults);
  end;

var
  OPPHelpSettingsForm: TOPPHelpSettingsForm;

implementation

uses
  commctrl,
  OPP.Help.predicate, OPP.Help.System.Types,
  OPP.Help.log,
  OPP.Help.System.error,
  OPP.Help.System.Files,

  OPP.Help.Defaults.Codable,

  System.IOUtils, System.JSON, DBXJSONReflect, REST.JSON;

const
  kContext = 'OPPSettingsForm';
  kShortcutFilePathPropertyName = 'ShortcutFilePath';
  kHintsFilePathPropertyName = 'HintsFilePath';

const
  SSettingsFileName = 'OPPHintTunning.settings';

resourcestring
  SEventSettingsWasSaved = 'settings was saved';
  SHintFilePathCaption = 'Путь к файлу подсказок';
  SShortFilePathCaption = 'Путь к файлу помощи';

{$R *.dfm}

class function TOPPHelpSettingsForm.GetUserDefaults: TOPPHelpDefaults;
var
  fOPPDecoder: TOPPCoder<TOPPHelpDefaults>;
begin
  fOPPDecoder := TOPPCoder<TOPPHelpDefaults>.Create;
  try
    try
      fOPPDecoder.DecodeFromJSON(SSettingsFileName, true, result);
    except
      on OPPFileNotFoundException do
      begin
        result := CreateUserDefaults;
        fOPPDecoder.EncodeToJSON(SSettingsFileName,result);
      end;
      on Error: Exception do
      begin
        eventLogger.Error(Error, kContext);
      end;
    end;
  finally
    fOPPDecoder.free;
  end;
end;

class procedure TOPPHelpSettingsForm.SetUserDefaults(ADefaults: TOPPHelpDefaults);
var
  fOPPDecoder: TOPPCoder<TOPPHelpDefaults>;
begin
  fOPPDecoder := TOPPCoder<TOPPHelpDefaults>.Create;
  try
    try
      fOPPDecoder.EncodeToJSON(SSettingsFileName, ADefaults);
    except
      on Error: Exception do
      begin
        eventLogger.Error(Error, kContext);
      end;
    end;
  finally
    fOPPDecoder.free;
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

  fValue.propertyCaption := cxListView1.Selected.Caption;
  fValue.propertyValue := cxListView1.Selected.Subitems[0];
  fValue.propertyName := cxListView1.Selected.Subitems[1];

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

class function TOPPHelpSettingsForm.CreateUserDefaults: TOPPHelpDefaults;
const
  SDefaultHintData = '.\Документация\hint.data';
  SDefaultHelpData = '.\Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf';
begin
  result := TOPPHelpDefaults.Create;
  result.HintsFilePath := SDefaultHintData;
  result.ShortcutFilePath := SDefaultHelpData;
end;

procedure TOPPHelpSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  saveSettings();
end;

procedure TOPPHelpSettingsForm.FormCreate(Sender: TObject);
begin
  self.Defaults := TOPPHelpSettingsForm.GetUserDefaults;
end;

procedure TOPPHelpSettingsForm.OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
begin
  if not Saved then
    exit;
  if AValue.propertyName = kHintsFilePathPropertyName then
    ffDefaults.HintsFilePath := AValue.propertyValue;
  if AValue.propertyName = kShortcutFilePathPropertyName then
    ffDefaults.ShortcutFilePath := AValue.propertyValue;
  cxListView1.loadSettings(self.Defaults);
end;

procedure TOPPHelpSettingsForm.saveSettings();
begin
  TOPPHelpSettingsForm.SetUserDefaults(self.Defaults);
end;

procedure TOPPHelpSettingsForm.setDefaults(const AValue: TOPPHelpDefaults);
begin
  ffDefaults := AValue;
  cxListView1.loadSettings(AValue);
end;

{ TOPPCxListView }

procedure TOPPCxListView.loadSettings(ASettings: TOPPHelpDefaults);
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
