unit OPP.Buffer.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, Datasnap.DBClient, System.Actions, Vcl.ActnList, cxGridLevel, cxClasses, cxGridCustomView, cxGrid,
  Vcl.Menus, JvComponentBase, JvClipboardMonitor,
  OPP.Buffer.Manager;

type
  TOPPBufferFormOnApply = reference to procedure(AText: String);

  TOPPBufferForm = class(TForm)
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    ActionList1: TActionList;
    actionClose: TAction;
    DataSource1: TDataSource;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    actionClose1: TMenuItem;
    actionExportSettings: TAction;
    actionImportSettings: TAction;
    actionExportBuffer: TAction;
    actionImportBuffer: TAction;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N10: TMenuItem;
    actionNewRecord: TAction;
    actionDeleteRecord: TAction;
    actionWipeRecords: TAction;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    actionShowSettings: TAction;
    N15: TMenuItem;
    actionLoadRecords: TAction;
    actionSaveRecords: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    actionTurnEditMode: TAction;
    N9: TMenuItem;
    menuItemIsEditMode: TMenuItem;
    actionApplySelection: TAction;
    actionMultiSelectMode: TAction;
    menuMultiSelectMode: TMenuItem;
    procedure actionApplySelectionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionExportBufferExecute(Sender: TObject);
    procedure actionExportSettingsExecute(Sender: TObject);
    procedure actionImportBufferExecute(Sender: TObject);
    procedure actionLoadRecordsExecute(Sender: TObject);
    procedure actionMultiSelectModeExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionSaveRecordsExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionTurnEditModeExecute(Sender: TObject);
    procedure actionWipeRecordsExecute(Sender: TObject);
    procedure ClientDataSet1CalcFields(DataSet: TDataSet);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure JvClipboardMonitor1Change(Sender: TObject);
    procedure cxGrid1DBTableView1DataControllerDataChanged(Sender: TObject);
  protected
  private
    fIsEditMode: Boolean;
    fOnApply: TOPPBufferFormOnApply;
    fIsMultiSelectMode: Boolean;
    procedure setIsEditMode(const Value: Boolean);
    procedure SetIsMultiSelectMode(const Value: Boolean);
    function GetHasRecords: Boolean;
    function GetCanDeleteRecord: Boolean;
    procedure ReloadActionsVisibility;
    { Private declarations }

    property IsEditMode: Boolean read fIsEditMode write setIsEditMode default false;
    property IsMultiSelectMode: Boolean read fIsMultiSelectMode write SetIsMultiSelectMode default false;
    property CanDeleteRecord: Boolean read GetCanDeleteRecord;
    property HasRecords: Boolean read GetHasRecords;
  public
    { Public declarations }
    property OnApply: TOPPBufferFormOnApply read fOnApply write fOnApply;
  end;

var
  OPPBufferForm: TOPPBufferForm;

implementation

uses
  Vcl.Clipbrd,
  OPP.Help.Log,
  OPP.Buffer.Settings.Form,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Help.System.Clipboard;

const
  kContext = 'TOPPBufferForm';

{$R *.dfm}

procedure TOPPBufferForm.actionApplySelectionExecute(Sender: TObject);
var
  fData: String;
begin
  if Assigned(fOnApply) then
  begin
    fData := DataSource1.DataSet.FieldByName('data').AsString;
    fOnApply(fData);
  end;
  Close;
end;

procedure TOPPBufferForm.FormCreate(Sender: TObject);
begin
  self.IsEditMode := false;
  self.readFormState;
  DataSource1.DataSet := TClientDataset(oppBufferManager.DataSet);
end;

procedure TOPPBufferForm.actionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferForm.actionDeleteRecordExecute(Sender: TObject);
begin
  try
    cxGrid1DBTableView1.DataController.BeginFullUpdate;
    cxGrid1DBTableView1.DataController.DeleteSelection;
    cxGrid1DBTableView1.DataController.EndFullUpdate;

  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPBufferForm.actionExportBufferExecute(Sender: TObject);
var
  fDefaultFilePath: String;
begin
  fDefaultFilePath := oppBufferManager.Settings.GetCurrentFilePath;
  if Length(fDefaultFilePath) = 0 then
  begin
    SaveDialog1.FileName := oppBufferManager.Settings.GetDefaultFilePath;
    if SaveDialog1.Execute(self.Handle) then
    begin
      oppBufferManager.SetRecordsStorageFileName(SaveDialog1.FileName);
      oppBufferManager.SaveRecords();
    end;
  end else begin
    oppBufferManager.SaveRecords();
  end;
end;

procedure TOPPBufferForm.actionExportSettingsExecute(Sender: TObject);
begin
  //
end;

procedure TOPPBufferForm.actionImportBufferExecute(Sender: TObject);
var
  fDefaultFilePath: String;
begin
  fDefaultFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath('OPPBufferManager.oppclipboarddata');
  OpenDialog1.FileName := fDefaultFilePath;
  if OpenDialog1.Execute(self.Handle) then
  begin
    oppBufferManager.SetRecordsStorageFileName(OpenDialog1.FileName);
    oppBufferManager.LoadRecords();
  end;
end;

procedure TOPPBufferForm.actionLoadRecordsExecute(Sender: TObject);
begin
  //
end;

procedure TOPPBufferForm.actionMultiSelectModeExecute(Sender: TObject);
begin
  self.IsMultiSelectMode := not self.IsMultiSelectMode;
end;

procedure TOPPBufferForm.actionNewRecordExecute(Sender: TObject);
begin
  oppBufferManager.AddEmpty();
end;

procedure TOPPBufferForm.actionSaveRecordsExecute(Sender: TObject);
begin
  //
end;

procedure TOPPBufferForm.actionShowSettingsExecute(Sender: TObject);
var
  fSettingsForm: TOPPBufferSettingsForm;
begin

  fSettingsForm := TOPPBufferSettingsForm.Create(self);
  try
    fSettingsForm.ShowModal;
  finally
    fSettingsForm.Free;
  end;

end;

procedure TOPPBufferForm.actionTurnEditModeExecute(Sender: TObject);
begin
  self.IsEditMode := not self.IsEditMode;
end;

procedure TOPPBufferForm.actionWipeRecordsExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.DataController.BeginFullUpdate;
  cxGrid1DBTableView1.DataController.SelectAll;
  cxGrid1DBTableView1.DataController.DeleteSelection;
  cxGrid1DBTableView1.DataController.EndFullUpdate;
end;

procedure TOPPBufferForm.ClientDataSet1CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('order').AsInteger := DataSet.RecNo;
end;

procedure TOPPBufferForm.cxGrid1DBTableView1DataControllerDataChanged(Sender: TObject);
begin
  self.reloadActionsVisibility;
end;

procedure TOPPBufferForm.FormActivate(Sender: TObject);
var
  done: Boolean;
begin
  cxGrid1.SetFocus;
  // cxGrid1DBTableView1.DataController.FocusControl(0,done);
  // self.ActiveControl := cxGrid1;
end;

procedure TOPPBufferForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oppBufferManager.SaveRecords();
  DataSource1.DataSet := nil;
  self.saveFormState;
end;

procedure TOPPBufferForm.FormResize(Sender: TObject);
begin
  cxGrid1DBTableView1.Columns[1].Width := cxGrid1.Width - cxGrid1DBTableView1.Columns[2].Width - cxGrid1DBTableView1.Columns[0].Width - 2;
end;

function TOPPBufferForm.GetCanDeleteRecord: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.RecordCount > 0) and (cxGrid1DBTableView1.DataController.RecNo >= 0);
end;

function TOPPBufferForm.GetHasRecords: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.RecordCount > 0);
end;

procedure TOPPBufferForm.JvClipboardMonitor1Change(Sender: TObject);
var
  Data: THandle;
  Buffer: Pointer;
  Size: LongInt;
  Stream: TStream;
begin
  Stream := TStream.Create;

  try
    Clipboard.Open;
    try
      Data := GetClipboardData(CF_TEXT);
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          // (rom) added handling of Format and Size!
          Size := GlobalSize(Data);
          Stream.Write(Format, SizeOf(Word));
          Stream.Write(Size, SizeOf(LongInt));
          Stream.Write(Buffer^, Size);
        finally
          GlobalUnlock(Data);
        end;
      end;
    finally
      Clipboard.Close;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TOPPBufferForm.ReloadActionsVisibility;
begin
  menuItemIsEditMode.Checked := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Deleting := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Editing := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Inserting := fIsEditMode;
  cxGrid1DBTableView1.OptionsSelection.CellSelect := fIsEditMode;
  menuMultiSelectMode.Enabled := fIsEditMode and self.HasRecords;
  actionMultiSelectMode.Enabled := fIsEditMode and self.HasRecords;
  menuMultiSelectMode.Checked := fIsMultiSelectMode and self.HasRecords;
  actionNewRecord.Enabled := fIsEditMode;
  actionDeleteRecord.Enabled := fIsEditMode and self.CanDeleteRecord;
  actionWipeRecords.Enabled := fIsMultiSelectMode and self.HasRecords;
end;

procedure TOPPBufferForm.setIsEditMode(const Value: Boolean);
begin
  fIsEditMode := Value;
  self.IsMultiSelectMode := false;
end;

procedure TOPPBufferForm.SetIsMultiSelectMode(const Value: Boolean);
begin
  fIsMultiSelectMode := Value;
  cxGrid1DBTableView1.OptionsSelection.MultiSelect := fIsMultiSelectMode;
  ReloadActionsVisibility;
end;

end.
