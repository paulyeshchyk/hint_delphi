unit OPP.Buffer.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Datasnap.DBClient, Vcl.ActnList, Vcl.Menus, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, cxGridLevel, cxClasses, cxGridCustomView, cxGrid,
  dxScrollbarAnnotations, cxTextEdit, cxContainer, cxButtons, dxDateRanges, cxGridDBTableView,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridCustomTableView, cxGridTableView,

  OPP.Buffer.Manager, OPP.Buffer.Manager.Settings;

type
  TOPPBufferFormOnApply = reference to procedure(AText: String);

  TOPPBufferForm = class(TForm)
    actionApplySelection: TAction;
    actionClose: TAction;
    actionClose1: TMenuItem;
    actionDeleteRecord: TAction;
    actionExportBuffer: TAction;
    actionExportSettings: TAction;
    actionImportBuffer: TAction;
    actionImportSettings: TAction;
    ActionList1: TActionList;
    actionLoadRecords: TAction;
    actionMultiSelectMode: TAction;
    actionNewRecord: TAction;
    actionSaveRecords: TAction;
    actionShowSettings: TAction;
    actionTurnEditMode: TAction;
    actionWipeRecords: TAction;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    cxGrid1Level1: TcxGridLevel;
    DataSource1: TDataSource;
    MainMenu1: TMainMenu;
    menuItemIsEditMode: TMenuItem;
    menuMultiSelectMode: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    actionMarkAsFixed: TAction;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    actionMarkAsNonFixed: TAction;
    N24: TMenuItem;
    N25: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    procedure actionApplySelectionExecute(Sender: TObject);
    procedure actionClose1Click(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionExportBufferExecute(Sender: TObject);
    procedure actionExportSettingsExecute(Sender: TObject);
    procedure actionImportBufferExecute(Sender: TObject);
    procedure actionLoadRecordsExecute(Sender: TObject);
    procedure actionMarkAsFixedExecute(Sender: TObject);
    procedure actionMarkAsNonFixedExecute(Sender: TObject);
    procedure actionMultiSelectModeExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionSaveRecordsExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionTurnEditModeExecute(Sender: TObject);
    procedure actionWipeRecordsExecute(Sender: TObject);
    procedure ClientDataSet1CalcFields(DataSet: TDataSet);
    procedure cxGrid1DBTableView1CellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure cxGrid1DBTableView1DataControllerDataChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure JvClipboardMonitor1Change(Sender: TObject);
    procedure cxGrid1DBTableView1Column2PropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure cxGrid1DBTableView1DataControllerSortingChanged(Sender: TObject);
  private
    fSettings: IOPPBufferManagerSettings;

    fIsEditMode: Boolean;
    fIsMultiSelectMode: Boolean;
    fOnApply: TOPPBufferFormOnApply;
    function GetHasRecords: Boolean;
    function GetHasSelectedRecord: Boolean;
    procedure ReloadActionsVisibility;
    procedure setIsEditMode(const Value: Boolean);
    procedure SetIsMultiSelectMode(const Value: Boolean);
    property HasRecords: Boolean read GetHasRecords;
    property HasSelectedRecord: Boolean read GetHasSelectedRecord;

    procedure ColumnSortSave;
    procedure ColumnSortRead;

    { Private declarations }

    property IsEditMode: Boolean read fIsEditMode write setIsEditMode default false;
    property IsMultiSelectMode: Boolean read fIsMultiSelectMode write SetIsMultiSelectMode default false;
  public
    class procedure ShowForm(AOwner: TControl); overload;
    class procedure ShowForm(AOwner: TControl; AControl: TControl); overload;
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
  OPP.Help.System.Clipboard,

  OPP.Buffer.Manager.Settings.Data,

  System.TypInfo, System.Rtti;

resourcestring
  SDuplicatedRecord = 'Такая запись уже есть в списке';

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

procedure TOPPBufferForm.actionClose1Click(Sender: TObject);
begin
  Close;
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

procedure TOPPBufferForm.actionMarkAsFixedExecute(Sender: TObject);
var
  i: Integer;
  rowIndex: Integer;
begin
  try
    cxGrid1DBTableView1.DataController.BeginFullUpdate;
    for i := 0 to cxGrid1DBTableView1.DataController.GetSelectedCount - 1 do
    begin
      rowIndex := cxGrid1DBTableView1.DataController.GetSelectedRowIndex(i);
      cxGrid1DBTableView1.DataController.SetValue(rowIndex, 2, true);
    end;
    cxGrid1DBTableView1.DataController.EndFullUpdate;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;

end;

procedure TOPPBufferForm.actionMarkAsNonFixedExecute(Sender: TObject);
var
  i: Integer;
  rowIndex: Integer;
begin
  try
    cxGrid1DBTableView1.DataController.BeginFullUpdate;
    for i := 0 to cxGrid1DBTableView1.DataController.GetSelectedCount - 1 do
    begin
      rowIndex := cxGrid1DBTableView1.DataController.GetSelectedRowIndex(i);
      cxGrid1DBTableView1.DataController.SetValue(rowIndex, 2, false);
    end;
    cxGrid1DBTableView1.DataController.EndFullUpdate;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
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
    fSettingsForm.onLeaveRecordsCount := procedure(ARecordsCountToLeave: Integer)
      begin
        oppBufferManager.RemoveRecordsAfter(ARecordsCountToLeave);
      end;
    fSettingsForm.onBufferSettingsLoad := function(): IOPPBufferManagerSettings
      begin
        result := oppBufferManager.Settings;
      end;

    fSettingsForm.onBufferSettingsSave := procedure(AValue: IOPPBufferManagerSettings)
      var
        fFilePath: String;
      begin
        try
          AValue.Save;
        except
          on E: Exception do
          begin
            fFilePath := AValue.GetDefaultFilePath;
            ShowMessage(Format('Невозможно сохранить настройки. Файл %s заблокирован. Обратитесь к администратору.', [fFilePath]));
          end;

        end;
      end;

    fSettingsForm.ShowModal;
  finally
    FreeAndNil(fSettingsForm);
  end;
end;

procedure TOPPBufferForm.actionTurnEditModeExecute(Sender: TObject);
var
  fEditingController: TcxGridTableEditingController;
begin
  fEditingController := cxGrid1DBTableView1.Controller.EditingController;
  if self.IsEditMode and (fEditingController <> nil) then
  begin
    fEditingController.HideEdit(true);
  end;
  self.IsEditMode := not self.IsEditMode;
end;

procedure TOPPBufferForm.actionWipeRecordsExecute(Sender: TObject);
begin
  try
    cxGrid1DBTableView1.DataController.BeginFullUpdate;
    cxGrid1DBTableView1.DataController.SelectAll;
    cxGrid1DBTableView1.DataController.DeleteSelection;
    cxGrid1DBTableView1.DataController.EndFullUpdate;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPBufferForm.ClientDataSet1CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('order').AsInteger := DataSet.RecNo;
end;

procedure TOPPBufferForm.ColumnSortRead;
var
  fColumnSort: TArray<TOPPBufferManagerSettingsColumnSort>;
  i: integer;
  fItem: TcxCustomGridTableItem;
begin
  fColumnSort := fSettings.GetColumnSort;
  for i := 0 to Length(fColumnSort) - 1 do
  begin
    fItem := cxGrid1DBTableView1.DataController.GetItemByFieldName(fColumnSort[i].FieldName);
    fItem.SortIndex := fColumnSort[i].SortIndex;
    fItem.SortOrder := TcxDataSortOrder(fColumnSort[i].SortType);
  end;
end;

procedure TOPPBufferForm.ColumnSortSave;
var
  cnt: Integer;
  i: Integer;
  sortIndex: Integer;
  sortType: TcxDataSortOrder;
  str: String;
  sortRecords: TArray<TOPPBufferManagerSettingsColumnSort>;
begin
  cnt := cxGrid1DBTableView1.DataController.ItemCount;

  SetLength(SortRecords, cnt);
  try

    for i := 0 to cnt - 1 do
    begin
      sortRecords[i].FieldName := cxGrid1DBTableView1.DataController.GetItemField(i).FieldName;
      sortRecords[i].SortIndex := cxGrid1DBTableView1.DataController.GetItemSortingIndex(i);
      sortRecords[i].SortType := Integer(cxGrid1DBTableView1.DataController.GetItemSortOrder(i));
    end;

    fSettings.SetColumnSort(sortRecords);

  finally
    SetLength(SortRecords, 0);
  end;

end;

procedure TOPPBufferForm.cxGrid1DBTableView1CellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  if actionApplySelection.Enabled then
    actionApplySelection.Execute;
end;

procedure TOPPBufferForm.cxGrid1DBTableView1Column2PropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := false;
  if oppBufferManager.DataSet.HasTheSameValue(DisplayValue) then
  begin
    Error := true;
    ErrorText := SDuplicatedRecord;
  end;
end;

procedure TOPPBufferForm.cxGrid1DBTableView1DataControllerDataChanged(Sender: TObject);
begin
  self.ReloadActionsVisibility;
end;

procedure TOPPBufferForm.cxGrid1DBTableView1DataControllerSortingChanged(Sender: TObject);
begin
  ColumnSortSave;
end;

procedure TOPPBufferForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  ReloadActionsVisibility;
end;

procedure TOPPBufferForm.FormActivate(Sender: TObject);
begin
  cxGrid1.SetFocus;
end;

procedure TOPPBufferForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fSettings.SetFormFrame(self.frame);
  oppBufferManager.SaveRecords();
  DataSource1.DataSet := nil;
end;

procedure TOPPBufferForm.FormCreate(Sender: TObject);
var ffFrame: TRect;
begin
  fSettings := oppBufferManager.Settings;
  ffFrame := fSettings.GetFormFrame;
  if not ffFrame.isEmpty then
    self.frame := ffFrame;


  self.IsEditMode := false;
  DataSource1.DataSet := TClientDataset(oppBufferManager.DataSet);

  ColumnSortRead;
end;

procedure TOPPBufferForm.FormResize(Sender: TObject);
begin
  cxGrid1DBTableView1.Columns[1].Width := cxGrid1.Width - cxGrid1DBTableView1.Columns[2].Width - cxGrid1DBTableView1.Columns[0].Width - 2;
end;

function TOPPBufferForm.GetHasRecords: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.RecordCount > 0);
end;

function TOPPBufferForm.GetHasSelectedRecord: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.RecordCount > 0) and (cxGrid1DBTableView1.DataController.RecNo >= 0);
end;

procedure TOPPBufferForm.JvClipboardMonitor1Change(Sender: TObject);
var
  Data: THandle;
  Buffer: Pointer;
  Size: LongInt;
  fStream: TStream;
begin
  fStream := TStream.Create;
  try
    Clipboard.Open;
    try
      Data := GetClipboardData(CF_TEXT);
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          Size := GlobalSize(Data);
          fStream.Write(Format, SizeOf(Word));
          fStream.Write(Size, SizeOf(LongInt));
          fStream.Write(Buffer^, Size);
        finally
          GlobalUnlock(Data);
        end;
      end;
    finally
      Clipboard.Close;
    end;
  finally
    fStream.Free;
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
  actionMarkAsFixed.Enabled := fIsEditMode and self.HasRecords;
  actionMarkAsNonFixed.Enabled := fIsEditMode and self.HasRecords;
  actionNewRecord.Enabled := fIsEditMode;
  actionDeleteRecord.Enabled := fIsEditMode and self.HasSelectedRecord;
  actionWipeRecords.Enabled := fIsMultiSelectMode and self.HasRecords;
  actionApplySelection.Enabled := (not fIsEditMode) and self.HasSelectedRecord and Assigned(OnApply);
  actionClose.Enabled := (not fIsEditMode);
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

class procedure TOPPBufferForm.ShowForm(AOwner: TControl);
var
  fForm: TOPPBufferForm;
begin
  fForm := TOPPBufferForm.Create(AOwner);
  try
    fForm.ShowModal;
  finally
    FreeAndNil(fForm);
  end;
end;

class procedure TOPPBufferForm.ShowForm(AOwner: TControl; AControl: TControl);
var
  fForm: TOPPBufferForm;

  function HasTextProp(AControl: TControl): Boolean;
  var
    Ctx: TRttiContext;
    Prop: TRttiProperty;
  begin
    Prop := Ctx.GetType(AControl.ClassType).GetProperty('Text');
    result := (Prop <> nil) and (Prop.Visibility in [mvPublic, mvPublished]);
  end;

  procedure SetTextProp(AControl: TControl; AText: String);
  var
    Ctx: TRttiContext;
    Prop: TRttiProperty;
  begin
    Prop := Ctx.GetType(AControl.ClassType).GetProperty('Text');
    Prop.SetValue(AControl, AText);
  end;

begin
  if not HasTextProp(AControl) then
    exit;

  fForm := TOPPBufferForm.Create(AOwner);
  try
    fForm.OnApply := procedure(AData: String)
      var
        Ctx: TRttiContext;
        Prop: TRttiProperty;
      begin
        Prop := Ctx.GetType(AControl.ClassType).GetProperty('Text');
        Prop.SetValue(AControl, AData);
      end;
    fForm.ShowModal;
  finally
    fForm.Free;
  end;

end;

end.
