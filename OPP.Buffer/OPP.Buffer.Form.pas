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

  cxGridDBDataDefinitions,

  OPP.Buffer.Manager.Settings.Data,

  OPP.Buffer.Manager, OPP.Buffer.Manager.Settings, cxCheckBox;

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
    actionMarkAsInverted: TAction;
    N26: TMenuItem;
    procedure actionApplySelectionExecute(Sender: TObject);
    procedure actionClose1Click(Sender: TObject);
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
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
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

    property IsEditMode: Boolean read fIsEditMode write SetIsEditMode default false;
    property IsMultiSelectMode: Boolean read fIsMultiSelectMode write SetIsMultiSelectMode default false;
  public
    class procedure ShowForm(AOwner: TControl); overload;
    class procedure ShowForm(AOwner: TControl; AControl: TControl); overload;
    { Public declarations }
    property OnApply: TOPPBufferFormOnApply read fOnApply write fOnApply;
  end;

  TOPPDataControllerSortHelper = class helper for TcxGridDBTableView
    procedure OPPSetColumnsSort(AArray: TArray<TOPPBufferManagerSettingsColumnSort>);
    function OPPGetColumnsSort: TArray<TOPPBufferManagerSettingsColumnSort>;
    procedure OPPSetFixedMark(AColumnIndex: Integer; AMark: Boolean);
    procedure OPPSetInvertedFixedMark(AColumnIndex: Integer);
    procedure OPPDeleteSelected;
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
  OPP.Help.System.Control;

resourcestring
  SDuplicatedRecord = 'Такая запись уже есть в списке';
  SErrotCantSaveSettingsTemplate = 'Невозможно сохранить настройки. Файл %s заблокирован. Обратитесь к администратору.';
const
  kFieldNameOrder = 'order';
  kFieldNameData = 'data';
  kFileNameOPPBufferManagerOppclipboarddata = 'OPPBufferManager.oppclipboarddata';
  kContext = 'TOPPBufferForm';

{$R *.dfm}

procedure TOPPBufferForm.actionApplySelectionExecute(Sender: TObject);
var
  fData: String;
begin
  if Assigned(fOnApply) then
  begin
    fData := DataSource1.DataSet.FieldByName(kFieldNameData).AsString;
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
  cxGrid1DBTableView1.OPPSetFixedMark(2, true);
end;

procedure TOPPBufferForm.actionMarkAsInvertedExecute(Sender: TObject);
begin
    cxGrid1DBTableView1.OPPSetInvertedFixedMark(2);
end;

procedure TOPPBufferForm.actionMarkAsNonFixedExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.OPPSetFixedMark(2, false);
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

procedure TOPPBufferForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  ReloadActionsVisibility;
end;

procedure TOPPBufferForm.FormActivate(Sender: TObject);
begin
  cxGrid1.SetFocus;
  ColumnSortRead;
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
    self.frame := ffFrame;

  self.IsEditMode := false;
  DataSource1.DataSet := TClientDataset(oppBufferManager.DataSet);
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
  actionMarkAsInverted.Enabled := fIsEditMode and self.HasRecords;
  actionNewRecord.Enabled := fIsEditMode;
  actionDeleteRecord.Enabled := fIsEditMode and self.HasSelectedRecord;
  actionWipeRecords.Enabled := fIsMultiSelectMode and self.HasRecords;
  actionApplySelection.Enabled := (not fIsEditMode) and self.HasSelectedRecord and Assigned(OnApply);
  actionClose.Enabled := (not fIsEditMode);
end;

procedure TOPPBufferForm.setIsEditMode(const Value: Boolean);
var style :TcxStyle;
begin
  fIsEditMode := Value;
  self.IsMultiSelectMode := false;

  cxGrid1DBTableView1Column2.Properties.ReadOnly := not fIsEditMode;
  cxGrid1DBTableView1Column3.Properties.ReadOnly := not fIsEditMode;
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

begin
  if not AControl.HasTextProp() then
    exit;

  fForm := TOPPBufferForm.Create(AOwner);
  try
    fForm.OnApply := procedure(AData: String)
      begin
      AControl.SetTextProp(AData);
      end;
    fForm.ShowModal;
  finally
    fForm.Free;
  end;

end;

{ TOPPDataControllerSortHelper }

procedure TOPPDataControllerSortHelper.OPPSetFixedMark(AColumnIndex: Integer; AMark: Boolean);
var
  i: Integer;
  row: TcxCustomGridRow;
  rowIndex: Integer;
  recordIndex: Integer;
begin
  try
    self.DataController.BeginFullUpdate;
    for i := 0 to self.Controller.SelectedRowCount - 1 do
    begin
      row := self.controller.SelectedRows[i];
      recordIndex := row.RecordIndex;
      self.DataController.SetValue(recordIndex, AColumnIndex, AMark);
    end;
    self.DataController.EndFullUpdate;
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
  fMark : Boolean;
begin
  try
    self.DataController.BeginFullUpdate;
    for i := 0 to self.Controller.SelectedRowCount - 1 do
    begin
      row := self.controller.SelectedRows[i];
      recordIndex := row.RecordIndex;
      fMark := self.DataController.GetValue(recordIndex, AColumnIndex);

      self.DataController.SetValue(recordIndex, AColumnIndex, (not fMark));
    end;
    self.DataController.EndFullUpdate;
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
  i: integer;
  fItem: TcxCustomGridTableItem;
begin
  self.DataController.BeginFullUpdate;
  for i := 0 to Length(AArray) - 1 do
  begin
    fItem := self.DataController.GetItemByFieldName(AArray[i].FieldName);
    fItem.SortIndex := AArray[i].SortIndex;
    fItem.SortOrder := TcxDataSortOrder(AArray[i].SortOrder);
  end;
  self.DataController.EndFullUpdate;
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
begin

  cnt := self.ItemCount;
  SetLength(result, cnt);

  for i := 0 to cnt - 1 do
  begin
    fFieldName := self.DataController.GetItemField(i).FieldName;
    fItem := self.DataController.GetItemByFieldName(fFieldName);
    result[i].FieldName := fFieldName;
    result[i].SortIndex := fItem.SortIndex;
    result[i].SortOrder := Integer(fItem.SortOrder);
  end;
end;

end.
