unit OPP.Help.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  OPP.Help.Settings.Value.Editor,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxListView,
  Vcl.StdActns,
  OPP.Help.Settings.Manager;

type

  TOPPHelpSettingsForm = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    Button1: TButton;
    actionSave: TAction;
    cxListView1: TcxListView;
    actionEditValue: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actionEditValueExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    ffDefaults: TOPPHelpDefaults;
    procedure setDefaults(const AValue: TOPPHelpDefaults);
    procedure saveSettings();
    procedure OnSettingsRead(const AResult: TOPPHelpDefaults; error: Exception);
    procedure OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
  public
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
  OPP.Help.predicate, OPP.Help.System.Types,
  OPP.Help.log,
  OPP.Help.System.error,
  OPP.Help.System.Files,
  System.IOUtils, System.JSON, DBXJSONReflect, REST.JSON;

const
  kShortcutFilePathPropertyName = 'ShortcutFilePath';
  kHintsFilePathPropertyName = 'HintsFilePath';

resourcestring
  SHintFilePathCaption = 'Путь к файлу подсказок';
  SShortFilePathCaption = 'Путь к файлу помощи';

{$R *.dfm}

procedure TOPPHelpSettingsForm.FormCreate(Sender: TObject);
begin
  TOPPHelpSettingsManager.readSettings(OnSettingsRead);
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

procedure TOPPHelpSettingsForm.OnSettingsRead(const AResult: TOPPHelpDefaults; error: Exception);
begin
  if not assigned(error) then
  begin
    Defaults := AResult;
    exit;
  end;

  eventLogger.Error(error);

  TOPPHelpSettingsManager.defaultSettings(
    procedure(const AResult: TOPPHelpDefaults; error: Exception)
    begin
      if assigned(error) then
      begin
        eventLogger.Error(error);
        exit;
      end;
      self.Defaults := AResult;
    end);
end;

procedure TOPPHelpSettingsForm.saveSettings();
begin
  TOPPHelpSettingsManager.saveSettings(ffDefaults,
    procedure(error: Exception)
    begin
      if assigned(error) then
      begin
        eventLogger.Error(error);
        exit;
      end;
      eventLogger.Flow('settings was saved', 'OPPSettingsForm');
    end);
end;

procedure TOPPHelpSettingsForm.setDefaults(const AValue: TOPPHelpDefaults);
begin
  ffDefaults := AValue;
  cxListView1.loadSettings(AValue);
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

procedure TOPPHelpSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  saveSettings();
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
end;

end.
