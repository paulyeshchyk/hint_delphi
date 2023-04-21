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
    procedure actionApplySelectionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionExportBufferExecute(Sender: TObject);
    procedure actionExportSettingsExecute(Sender: TObject);
    procedure actionImportBufferExecute(Sender: TObject);
    procedure actionLoadRecordsExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionSaveRecordsExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionTurnEditModeExecute(Sender: TObject);
    procedure ClientDataSet1CalcFields(DataSet: TDataSet);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure JvClipboardMonitor1Change(Sender: TObject);
  protected
  private
    fIsEditMode: Boolean;
    fOnApply: TOPPBufferFormOnApply;
    procedure setIsEditMode(const Value: Boolean);
    { Private declarations }

    property IsEditMode: Boolean read fIsEditMode write setIsEditMode default false;
  public
    { Public declarations }
    property OnApply: TOPPBufferFormOnApply read fOnApply write fOnApply;
  end;

var
  OPPBufferForm: TOPPBufferForm;

implementation

uses
  Vcl.Clipbrd,
  OPP.Buffer.Settings.Form,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Help.System.Clipboard;

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

procedure TOPPBufferForm.ClientDataSet1CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('order').AsInteger := DataSet.RecNo;
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

procedure TOPPBufferForm.setIsEditMode(const Value: Boolean);
begin
  fIsEditMode := Value;
  menuItemIsEditMode.Checked := fIsEditMode;
  cxGrid1DBTableView1.Columns[1].Options.Editing := fIsEditMode;
  cxGrid1DBTableView1.Columns[2].Options.Editing := fIsEditMode;
end;

end.
