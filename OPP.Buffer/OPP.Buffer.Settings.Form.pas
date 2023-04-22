unit OPP.Buffer.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, cxButtons, Vcl.ExtCtrls, cxControls, cxContainer, cxEdit, cxCustomListBox, cxCheckListBox,
  cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, cxCheckBox, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, Vcl.ComCtrls, Datasnap.DBClient,
  OPP.Buffer.Manager.Settings;

type

  TOPPBufferManagerSettingsOnLoad = reference to function(): IOPPBufferManagerSettings;
  TOPPBufferManagerSettingsOnSave = reference to procedure(AValue: IOPPBufferManagerSettings);

  TOPPBufferSettingsForm = class(TForm)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    cxButton1: TcxButton;
    ActionList1: TActionList;
    actionClose: TAction;
    recordsCountLimitCheckbox: TcxCheckBox;
    recordsCountLimitEdit: TcxSpinEdit;
    cxLabel1: TcxLabel;
    GroupBox2: TGroupBox;
    AllowExternalsCheckBox: TcxCheckBox;
    actionUnlimitedRecordsCount: TAction;
    actionAddRecordsFromOtherApps: TAction;
    actionSaveSettings: TAction;
    CanSaveFormFrameCheckbox: TcxCheckBox;
    GroupBox3: TGroupBox;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxLabel2: TcxLabel;
    clipboardManagerShortcut: THotKey;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet2: TClientDataSet;
    ClientDataSet2SortTypeID: TSmallintField;
    ClientDataSet2SortTypeName: TStringField;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    ClientDataSet3: TClientDataSet;
    ClientDataSet3FieldNameID: TSmallintField;
    ClientDataSet3FieldNameCaption: TStringField;
    ClientDataSet1SortTypeID: TSmallintField;
    ClientDataSet1SortTypeCaption: TStringField;
    ClientDataSet1SortOrder: TSmallintField;
    ClientDataSet1FieldNameID: TSmallintField;
    ClientDataSet1FieldNameCaption: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionSaveSettingsExecute(Sender: TObject);
    procedure clipboardManagerShortcutChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure recordsCountLimitCheckboxPropertiesEditValueChanged(Sender: TObject);
    procedure recordsCountLimitEditPropertiesEditValueChanged(Sender: TObject);
    procedure CanSaveFormFrameCheckboxPropertiesEditValueChanged(Sender: TObject);
    procedure AllowExternalsCheckBoxPropertiesEditValueChanged(Sender: TObject);
  private
    fSettings: IOPPBufferManagerSettings;
    fOnBufferSettingsLoad: TOPPBufferManagerSettingsOnLoad;
    fOnBufferSettingsSave: TOPPBufferManagerSettingsOnSave;
    procedure DoReloadUI;
    procedure DoUpdateUI;
    procedure SetSettings(Value: IOPPBufferManagerSettings);
    { Private declarations }
    property Settings: IOPPBufferManagerSettings read fSettings write SetSettings;
  public
    { Public declarations }
    property onBufferSettingsLoad: TOPPBufferManagerSettingsOnLoad read fOnBufferSettingsLoad write fOnBufferSettingsLoad;
    property onBufferSettingsSave: TOPPBufferManagerSettingsOnSave read fOnBufferSettingsSave write fOnBufferSettingsSave;
  end;

var
  OPPBufferSettingsForm: TOPPBufferSettingsForm;

implementation

uses OPP.Help.System.Files, OPP.Help.Log, VarUtils;

{$R *.dfm}

procedure TOPPBufferSettingsForm.FormCreate(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile(TOPPHelpSystemFilesHelper.GetOPPSettingsPath('OPPBufferManager.Sort.Settings'));
  ClientDataSet3.LoadFromFile(TOPPHelpSystemFilesHelper.GetOPPSettingsPath('OPPBufferManager.Column.Settings'));
end;

procedure TOPPBufferSettingsForm.recordsCountLimitCheckboxPropertiesEditValueChanged(Sender: TObject);
begin
  fSettings.SetUseRecordsCountLimit(recordsCountLimitCheckbox.Checked);
  DoUpdateUI;
end;

procedure TOPPBufferSettingsForm.recordsCountLimitEditPropertiesEditValueChanged(Sender: TObject);
begin
  fSettings.SetRecordsCountLimit(recordsCountLimitEdit.Value);
end;

procedure TOPPBufferSettingsForm.SetSettings(Value: IOPPBufferManagerSettings);
begin
  fSettings := Value;
  DoReloadUI;
end;

procedure TOPPBufferSettingsForm.actionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferSettingsForm.actionSaveSettingsExecute(Sender: TObject);
begin

  ClientDataSet3.SaveToFile(TOPPHelpSystemFilesHelper.GetOPPSettingsPath('OPPBufferManager.Column.settings'));
  ClientDataSet1.SaveToFile(TOPPHelpSystemFilesHelper.GetOPPSettingsPath('OPPBufferManager.Sort.settings'));

  if assigned(fOnBufferSettingsSave) then
  begin
    fOnBufferSettingsSave(fSettings);
  end else begin
    eventLogger.Error('OnBufferSettingsSave is not assigned', 'TOPPBufferSettingsForm');
  end;

  Close;
end;

procedure TOPPBufferSettingsForm.AllowExternalsCheckBoxPropertiesEditValueChanged(Sender: TObject);
begin
  fSettings.SetIsExternalAllowed(AllowExternalsCheckBox.Checked);
end;

procedure TOPPBufferSettingsForm.CanSaveFormFrameCheckboxPropertiesEditValueChanged(Sender: TObject);
begin
  fSettings.SetCanSaveFormFrame(CanSaveFormFrameCheckbox.Checked);
end;

procedure TOPPBufferSettingsForm.DoReloadUI;
begin
  recordsCountLimitCheckbox.Checked := fSettings.GetUseRecordsCountLimit;
  recordsCountLimitEdit.Value := fSettings.GetRecordsCountLimit;
  AllowExternalsCheckBox.Checked := fSettings.GetIsExternalAllowed;
  CanSaveFormFrameCheckbox.Checked := fSettings.GetCanSaveFormFrame;
  clipboardManagerShortcut.HotKey := fSettings.GetShortCut;

  DoUpdateUI;
end;

procedure TOPPBufferSettingsForm.DoUpdateUI;
begin
  cxLabel1.Enabled := fSettings.GetUseRecordsCountLimit;
  recordsCountLimitEdit.Enabled := fSettings.GetUseRecordsCountLimit;
end;

procedure TOPPBufferSettingsForm.clipboardManagerShortcutChange(Sender: TObject);
begin
  fSettings.SetShortCut(clipboardManagerShortcut.HotKey);
end;

procedure TOPPBufferSettingsForm.FormActivate(Sender: TObject);
var
  fLoadedSettings: IOPPBufferManagerSettings;
begin
  if not assigned(fOnBufferSettingsLoad) then
  begin
    eventLogger.Error('fOnBufferSettingsLoad is not assigned');
    exit;
  end;
  fLoadedSettings := fOnBufferSettingsLoad();
  if not assigned(fLoadedSettings) then
  begin
    eventLogger.Error('settings was not loaded');
    exit;
  end;
  self.Settings := fLoadedSettings
end;

procedure TOPPBufferSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(fOnBufferSettingsSave) then
  begin
    fOnBufferSettingsSave(fSettings);
  end;
end;

end.
