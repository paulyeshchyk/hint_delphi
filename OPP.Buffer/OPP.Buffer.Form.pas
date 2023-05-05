unit OPP.Buffer.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Datasnap.DBClient, Vcl.ActnList, Vcl.Menus, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, cxGridLevel, cxClasses, cxGridCustomView, cxGrid,
  cxTextEdit, cxContainer, cxButtons, cxGridDBTableView,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridCustomTableView, cxGridTableView,

  cxGridDBDataDefinitions,

  OPP.Buffer.Manager.DataSet,
  OPP.Buffer.Manager.Settings.Data,
  OPP.Buffer.Clipboard,
  OPP.Buffer.Manager, OPP.Buffer.Manager.Settings, cxCheckBox, dxStatusBar, dxBar, Vcl.ExtCtrls, cxBlobEdit, cxImage;

type
  TOPPBufferFormOnApply = reference to procedure(ARecord: TOPPBufferManagerRecord; AClipboardControl: TWinControl);

  TOPPBufferForm = class(TForm)
    actionApplySelection: TAction;
    actionCloseByPressingEsc: TAction;
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
    actionMarkAsNonFixed: TAction;
    N25: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    actionMarkAsInverted: TAction;
    dxStatusBar1: TdxStatusBar;
    Panel1: TPanel;
    dxBarDockControl1: TdxBarDockControl;
    Panel2: TPanel;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarLargeButton4: TdxBarLargeButton;
    dxBarLargeButton5: TdxBarLargeButton;
    dxBarManager1Bar2: TdxBar;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarManager1Bar3: TdxBar;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton1: TdxBarButton;
    dxBarSeparator1: TdxBarSeparator;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarSeparator2: TdxBarSeparator;
    dxBarButton4: TdxBarButton;
    dxBarSubItem2: TdxBarSubItem;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarSeparator3: TdxBarSeparator;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarSeparator4: TdxBarSeparator;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarButton12: TdxBarButton;
    dxBarSeparator5: TdxBarSeparator;
    dxBarButton13: TdxBarButton;
    dxBarButton14: TdxBarButton;
    actionClose: TAction;
    cxGrid1DBTableView1Column4: TcxGridDBColumn;
    procedure actionApplySelectionExecute(Sender: TObject);
    procedure actionClose1Click(Sender: TObject);
    procedure actionCloseByPressingEscExecute(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionExportBufferExecute(Sender: TObject);
    procedure actionExportSettingsExecute(Sender: TObject);
    procedure actionImportBufferExecute(Sender: TObject);
    procedure actionLoadRecordsExecute(Sender: TObject);
    procedure actionMarkAsFixedExecute(Sender: TObject);
    procedure actionMarkAsInvertedExecute(Sender: TObject);
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
    procedure cxGrid1DBTableView1Column2PropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure cxGrid1DBTableView1Editing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
    procedure cxGrid1DBTableView1SelectionChanged(Sender: TcxCustomGridTableView);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure DataSource1StateChange(Sender: TObject);
  private
    fSettings: IOPPBufferManagerSettings;

    fIsEditMode: Boolean;
    fIsMultiSelectMode: Boolean;
    fOnApply: TOPPBufferFormOnApply;
    fClipboardControl: TWinControl;
    function GetHasRecords: Boolean;
    function GetHasSelectedRecord: Boolean;
    procedure ReloadActionsVisibility;
    procedure setIsEditMode(const Value: Boolean);
    procedure SetIsMultiSelectMode(const Value: Boolean);
    function GetHasSelectedFewRecords: Boolean;
    function GetSelectedRecordsCountText: String;
    procedure SetOnApply(const Value: TOPPBufferFormOnApply);
    function GetDataset: IOPPBufferManagerDataset;
    property HasRecords: Boolean read GetHasRecords;
    property HasSelectedRecord: Boolean read GetHasSelectedRecord;
    property HasSelectedFewRecords: Boolean read GetHasSelectedFewRecords;
    property SelectedRecordsCountText: String read GetSelectedRecordsCountText;
    property Dataset:IOPPBufferManagerDataset read GetDataset;

    procedure ColumnSortSave;
    procedure ColumnSortRead;
    procedure ColumnSizeChange;

    class procedure OnApplyData(ARecord: TOPPBufferManagerRecord; AClipboardControl: TWinControl);

    { Private declarations }

    property IsEditMode: Boolean read fIsEditMode write setIsEditMode default false;
    property IsMultiSelectMode: Boolean read fIsMultiSelectMode write SetIsMultiSelectMode default false;

  public
    class procedure ShowForm(AOwner: TControl); overload;
    class procedure ShowForm(AOwner: TControl; AControl: TWinControl); overload;
    { Public declarations }
    property OnApply: TOPPBufferFormOnApply read fOnApply write SetOnApply;
    property ClipboardControl: TWinControl read fClipboardControl write fClipboardControl;
  end;

  TOPPDataControllerSortHelper = class helper for TcxGridDBTableView
    procedure OPPSetColumnsSort(AArray: TArray<TOPPBufferManagerSettingsColumnSort>);
    function OPPGetColumnsSort: TArray<TOPPBufferManagerSettingsColumnSort>;
    procedure OPPSetFixedMark(AFieldName: String; AMark: Boolean);
    procedure OPPSetInvertedFixedMark(AColumnIndex: Integer);
    procedure OPPDeleteSelected;
  end;

  TOPPBufferFormHelper = class
    class procedure injectionShowForm;
  end;

  TOPPcxGridDBColumnHelper = class helper for TcxGridDBColumn
    procedure FillRestOfTheSpace;
  end;

var
  OPPBufferForm: TOPPBufferForm;

implementation

uses
  Vcl.Clipbrd, System.StrUtils,
  OPP.Help.Log,
  OPP.Buffer.Settings.Form,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Help.System.Clipboard,
  OPP.Help.System.Control,

  OPPConfiguration,

  OPP.Keyboard.Shortcut.Manager;

resourcestring
  SDuplicatedRecord = 'Такая запись уже есть в списке';
  SErrotCantSaveSettingsTemplate = 'Невозможно сохранить настройки. Файл %s заблокирован. Обратитесь к администратору.';
  SDeleteRecord = 'Удалить текущую';
  SDeleteSelectedRecords = 'Удалить выбранные';
  SEditMode = 'Режим редактирования';
  SPreviewMode = '';
  SEditMultirecordSelection = 'Режим выбора нескольких записей';
  SSelectedRecordsTemplate = 'Выбрано: %d';

const
  kContext = 'TOPPBufferForm';
  kFieldNameOrder = 'order';
  kFieldNameData = 'data';
  kFileNameOPPBufferManagerOppclipboarddata = 'OPPBufferManager.oppclipboarddata';

{$R *.dfm}

procedure TOPPBufferForm.actionApplySelectionExecute(Sender: TObject);
var
  fBytes: TArray<Byte>;
begin
  if not Assigned(fOnApply) then
  begin
    Close;
    exit;
  end;


  Dataset.ExtractRecord(
    procedure(theRecord: TOPPBufferManagerRecord)
    begin
      if Assigned(theRecord) then
      begin
        fOnApply(theRecord, fClipboardControl);
        Close;
      end;
    end);
end;

procedure TOPPBufferForm.actionClose1Click(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferForm.actionCloseByPressingEscExecute(Sender: TObject);
begin
  if cxGrid1DBTableView1Column2.Editing then
  begin
    cxGrid1DBTableView1Column2.Editing := false;
    exit;
  end;
  Close;
end;

procedure TOPPBufferForm.actionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferForm.actionDeleteRecordExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.OPPDeleteSelected;
end;

procedure TOPPBufferForm.actionExportBufferExecute(Sender: TObject);
begin
  SaveDialog1.FileName := oppBufferManager.Settings.GetDefaultFilePath;
  if SaveDialog1.Execute(self.Handle) then
  begin
    oppBufferManager.SaveRecords(SaveDialog1.FileName);
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
  fDefaultFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(kFileNameOPPBufferManagerOppclipboarddata);
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
begin
  cxGrid1DBTableView1.OPPSetFixedMark('isFixed', true);
end;

procedure TOPPBufferForm.actionMarkAsInvertedExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.OPPSetInvertedFixedMark(2);
end;

procedure TOPPBufferForm.actionMarkAsNonFixedExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.OPPSetFixedMark('isFixed', false);
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
            ShowMessage(Format(SErrotCantSaveSettingsTemplate, [fFilePath]));
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
  cxGrid1DBTableView1.DataController.SelectAll;
  cxGrid1DBTableView1.OPPDeleteSelected;
end;

procedure TOPPBufferForm.ClientDataSet1CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName(kFieldNameOrder).AsInteger := DataSet.RecNo;
end;

procedure TOPPBufferForm.ColumnSizeChange;
var
  i: Integer;
  fColumn: TcxGridDBColumn;
const
  columnDataBindingNames: array [0 .. 3] of String = ('SortIndex', '_TYPE', 'data', 'isFixed');
begin
  for i := 0 to cxGrid1DBTableView1.ColumnCount - 1 do
  begin
    fColumn := cxGrid1DBTableView1.Columns[i];
    case IndexStr(fColumn.DataBinding.FieldName, columnDataBindingNames) of
      0: // SortIndex
        begin
          fColumn.ApplyBestFit(true);
        end;
      1: // _TYPE
        begin
          fColumn.Width := 22;
        end;
      2: // data
        begin
          fColumn.FillRestOfTheSpace;
        end;
      3: // isFixed
        begin
          fColumn.ApplyBestFit(true);
        end;
    end;
  end;
end;

procedure TOPPBufferForm.ColumnSortRead;
begin
  cxGrid1DBTableView1.OPPSetColumnsSort(fSettings.GetColumnSort);
end;

procedure TOPPBufferForm.ColumnSortSave;
var
  sortRecords: TArray<TOPPBufferManagerSettingsColumnSort>;
begin
  sortRecords := cxGrid1DBTableView1.OPPGetColumnsSort;
  try
    fSettings.SetColumnSort(sortRecords);
  finally
    SetLength(sortRecords, 0);
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

procedure TOPPBufferForm.cxGrid1DBTableView1Editing(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; var AAllow: Boolean);
begin
  ReloadActionsVisibility;
end;

procedure TOPPBufferForm.cxGrid1DBTableView1SelectionChanged(Sender: TcxCustomGridTableView);
begin
  ReloadActionsVisibility;
end;

procedure TOPPBufferForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // ReloadActionsVisibility;
  // ColumnSizeChange;
end;

procedure TOPPBufferForm.DataSource1StateChange(Sender: TObject);
begin
  ReloadActionsVisibility;
  // ColumnSizeChange;
end;

procedure TOPPBufferForm.FormActivate(Sender: TObject);
begin
  cxGrid1.SetFocus;
  ColumnSortRead;
  ColumnSizeChange;
end;

procedure TOPPBufferForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ColumnSortSave;
  fSettings.SetFormFrame(self.frame);
  oppBufferManager.SaveRecords();
  DataSource1.DataSet := nil;
end;

procedure TOPPBufferForm.FormCreate(Sender: TObject);
var
  ffFrame: TRect;
begin
  fSettings := oppBufferManager.Settings;
  ffFrame := fSettings.GetFormFrame;
  if not ffFrame.isEmpty then
  begin
    if ffFrame.Left < 0 then
    begin
      ffFrame.Width := ffFrame.Width + Abs(ffFrame.Left);
      ffFrame.Left := 0;
    end;
    if ffFrame.Left > Screen.WorkAreaWidth then
    begin
      ffFrame.Left := ffFrame.Left - 20;
    end;
    self.frame := ffFrame;
  end;

  self.IsEditMode := false;
  DataSource1.DataSet := TClientDataset(self.Dataset);

  cxGrid1DBTableView1Column4.RepositoryItem := Config.TypesNamesRepository;

end;

procedure TOPPBufferForm.FormResize(Sender: TObject);
begin
  ColumnSizeChange;
end;

function TOPPBufferForm.GetDataset: IOPPBufferManagerDataset;
begin
  result := oppBufferManager.Dataset;
end;

function TOPPBufferForm.GetHasRecords: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.RecordCount > 0);
end;

function TOPPBufferForm.GetHasSelectedRecord: Boolean;
begin
  result := (cxGrid1DBTableView1.DataController.FocusedRecordIndex >= 0);
end;

function TOPPBufferForm.GetSelectedRecordsCountText: String;
begin
  result := '';
  if not fIsMultiSelectMode then
    exit;
  if cxGrid1DBTableView1.Controller.SelectedRowCount <= 1 then
    exit;

  result := Format(SSelectedRecordsTemplate, [cxGrid1DBTableView1.Controller.SelectedRowCount]);
end;

function TOPPBufferForm.GetHasSelectedFewRecords: Boolean;
begin
  result := (cxGrid1DBTableView1.Controller.SelectedRowCount > 1);
end;

procedure TOPPBufferForm.ReloadActionsVisibility;
begin
  dxBarButton5.Down := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Deleting := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Editing := fIsEditMode;
  cxGrid1DBTableView1.OptionsData.Inserting := fIsEditMode;
  cxGrid1DBTableView1.OptionsSelection.CellSelect := fIsEditMode;
  dxBarButton6.Enabled := fIsEditMode and self.HasRecords;
  actionMultiSelectMode.Enabled := fIsEditMode and self.HasRecords;
  dxBarButton6.Down := fIsMultiSelectMode and self.HasRecords;
  actionMarkAsFixed.Enabled := fIsEditMode and self.HasRecords;
  actionMarkAsNonFixed.Enabled := fIsEditMode and self.HasRecords;
  actionMarkAsInverted.Enabled := fIsEditMode and self.HasRecords;
  actionNewRecord.Enabled := fIsEditMode;
  actionDeleteRecord.Enabled := fIsEditMode and self.HasSelectedRecord;
  actionDeleteRecord.Caption := ifThen(self.HasSelectedFewRecords, SDeleteSelectedRecords, SDeleteRecord);
  actionWipeRecords.Enabled := fIsMultiSelectMode and self.HasRecords and (not self.HasSelectedFewRecords);
  actionApplySelection.Enabled := (not fIsEditMode) and self.HasSelectedRecord and Assigned(OnApply);
  dxStatusBar1.Panels[0].Text := ifThen(not fIsEditMode, SPreviewMode, ifThen(fIsMultiSelectMode, SEditMultirecordSelection, SEditMode));
  dxStatusBar1.Panels[1].Text := ifThen(not fIsMultiSelectMode, '', self.SelectedRecordsCountText)
end;

procedure TOPPBufferForm.setIsEditMode(const Value: Boolean);
var
  style: TcxStyle;
begin
  fIsEditMode := Value;
  self.IsMultiSelectMode := false;
  dxBarLargeButton6.Down := fIsEditMode;

  cxGrid1DBTableView1Column2.Properties.ReadOnly := not fIsEditMode;
  cxGrid1DBTableView1Column3.Properties.ReadOnly := not fIsEditMode;
end;

procedure TOPPBufferForm.SetIsMultiSelectMode(const Value: Boolean);
begin
  fIsMultiSelectMode := Value;
  cxGrid1DBTableView1.OptionsSelection.MultiSelect := fIsMultiSelectMode;
  ReloadActionsVisibility;
end;

procedure TOPPBufferForm.SetOnApply(const Value: TOPPBufferFormOnApply);
begin
  fOnApply := Value;
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

class procedure TOPPBufferForm.OnApplyData(ARecord: TOPPBufferManagerRecord; AClipboardControl: TWinControl);
begin
  if not Assigned(AClipboardControl) then
  begin
    eventLogger.Warning('Not assigned clipboard control', kContext);
    exit;
  end;

  try
    oppBufferManager.WriteDataIntoControl(AClipboardControl, ARecord);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

class procedure TOPPBufferForm.ShowForm(AOwner: TControl; AControl: TWinControl);
var
  fForm: TOPPBufferForm;
  fCanApplyText: Boolean;
  mrResult: TModalResult;
begin

  fForm := TOPPBufferForm.Create(AOwner);
  try
    fCanApplyText := false;
    if Assigned(AControl) then
      fCanApplyText := AControl.HasTextProp();
    if fCanApplyText then
    begin
      fForm.ClipboardControl := AControl;
      fForm.OnApply := TOPPBufferForm.OnApplyData;
    end;
    try
      mrResult := fForm.ShowModal;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, 'TOPPBufferForm');
      end;
    end;
  finally
    fForm.Free;
  end;
end;

{ TOPPDataControllerSortHelper }

procedure TOPPDataControllerSortHelper.OPPSetFixedMark(AFieldName: String; AMark: Boolean);
var
  i, AColumnIndex: Integer;
  row: TcxCustomGridRow;
  rowIndex: Integer;
  recordIndex: Integer;
begin
  AColumnIndex := self.GetColumnByFieldName(AFieldName).Index;
  try
    self.DataController.Edit;
    for i := 0 to self.Controller.SelectedRowCount - 1 do
    begin
      row := self.Controller.SelectedRows[i];
      recordIndex := row.recordIndex;
      self.DataController.SetValue(recordIndex, AColumnIndex, AMark);
    end;
    self.DataController.PostEditingData;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'SetFixedMark');
    end;
  end;
end;

procedure TOPPDataControllerSortHelper.OPPSetInvertedFixedMark(AColumnIndex: Integer);
var
  i: Integer;
  row: TcxCustomGridRow;
  rowIndex: Integer;
  recordIndex: Integer;
  fMark: Boolean;
begin
  try
    self.DataController.Edit;
    for i := 0 to self.Controller.SelectedRowCount - 1 do
    begin
      row := self.Controller.SelectedRows[i];
      recordIndex := row.recordIndex;
      fMark := self.DataController.GetValue(recordIndex, AColumnIndex);

      self.DataController.SetValue(recordIndex, AColumnIndex, (not fMark));
    end;
    self.DataController.PostEditingData;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'SetFixedMark');
    end;
  end;
end;

procedure TOPPDataControllerSortHelper.OPPSetColumnsSort(AArray: TArray<TOPPBufferManagerSettingsColumnSort>);
var
  fColumnSort: TArray<TOPPBufferManagerSettingsColumnSort>;
  i: Integer;
  fItem: TcxCustomGridTableItem;
begin
  // self.DataController.BeginFullUpdate;
  for i := 0 to Length(AArray) - 1 do
  begin
    fItem := self.DataController.GetItemByFieldName(AArray[i].FieldName);
    fItem.SortIndex := AArray[i].SortIndex;
    fItem.SortOrder := TcxDataSortOrder(AArray[i].SortOrder);
  end;
  // self.DataController.EndFullUpdate;
  self.DataController.PostEditingData;
end;

procedure TOPPDataControllerSortHelper.OPPDeleteSelected;
begin
  try
    self.DataController.BeginFullUpdate;
    self.DataController.DeleteSelection;
    self.DataController.EndFullUpdate;

  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'DeleteSelected');
    end;
  end;
end;

function TOPPDataControllerSortHelper.OPPGetColumnsSort: TArray<TOPPBufferManagerSettingsColumnSort>;
var
  cnt: Integer;
  i: Integer;
  fItem: TcxCustomGridTableItem;
  fFieldName: String;
  fField: TField;
begin

  cnt := self.ItemCount;
  SetLength(result, cnt);

  for i := 0 to cnt - 1 do
  begin
    fItem := self.Items[i];
    if not(Assigned(fItem.DataBinding) and (fItem.DataBinding is TcxGridItemDBDataBinding)) then
      continue;
    fFieldName := TcxGridItemDBDataBinding(fItem.DataBinding).FieldName;
    result[i].FieldName := fFieldName;
    result[i].SortIndex := fItem.SortIndex;
    result[i].SortOrder := Integer(fItem.SortOrder);
  end;
end;

{ TOPPBufferFormHelper }

class procedure TOPPBufferFormHelper.injectionShowForm;
begin
  keyboardShortcutManager.registerHook(oppBufferManager.Settings.GetShortCut,
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if FindWindow('TOPPBufferForm', nil) = 0 then
          begin
            TOPPBufferForm.ShowForm(nil, Screen.ActiveControl);
          end
          else
            eventLogger.Debug('Cant run second instance');
        end);
    end);
end;

{ TOPPcxGridDBColumnHelper }

procedure TOPPcxGridDBColumnHelper.FillRestOfTheSpace;
var
  i: Integer;
  fColumn: TcxGridColumn;
  theWidth: Integer;
  thwWidthScrollBar: Integer;
begin
  thwWidthScrollBar := 0;
  if self.GridView.Site.VScrollBar.Visible then
    thwWidthScrollBar := self.GridView.Site.VScrollBar.Width;
  theWidth := self.GridView.Site.Width;
  theWidth := theWidth - thwWidthScrollBar;
  for i := 0 to self.GridView.ColumnCount - 1 do
  begin
    fColumn := self.GridView.Columns[i];
    if not Assigned(fColumn) or not(fColumn.Visible) or (fColumn = self) then
      continue;
    theWidth := theWidth - fColumn.Width;
  end;
  self.Width := theWidth;
end;

initialization

TOPPBufferFormHelper.injectionShowForm;

end.
