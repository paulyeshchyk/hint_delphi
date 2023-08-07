unit OPP.Price.Calculator.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Generics.Collections,
  OPP.Price.Methology, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, Datasnap.DBClient,
  cxInplaceContainer, cxTLData, cxDBTL, cxMaskEdit, Vcl.ComCtrls, dxtree, dxdbtree, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, cxDBData, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid,
  OPP.Price.Order,
  OPP.Price.Item,
  OPP.Price.Project,
  OPP.Price.Methology.List,
  OPP.Help.Vcl.PanelTrigger,
  cxPC, dxDockControl, dxDockPanel, dxBar, System.Actions, Vcl.ActnList, Vcl.Menus, cxVGrid, cxDBVGrid, dxStatusBar;

type
  TOPPPriceCalculator = class(TForm)
    TreeDatasource: TDataSource;
    MethologyDatasource: TDataSource;
    PriceItemDataset: TClientDataSet;
    PriceItemDatasetPriceItemID: TStringField;
    PriceItemDatasetPriceItemName: TStringField;
    PriceItemDatasetPriceItemDescription: TStringField;
    TreeDataset: TClientDataSet;
    TreeDatasetMethologyName: TStringField;
    TreeDatasetParentMethologyName: TStringField;
    TreeDatasetCurrentMethologyName: TStringField;
    TreeDatasetIdentificator: TStringField;
    dxDockingManager1: TdxDockingManager;
    dxDockPanel1: TdxDockPanel;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxDockPanel2: TdxDockPanel;
    cxDBTreeList1: TcxDBTreeList;
    cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1Level1: TcxGridLevel;
    dxDockPanel3: TdxDockPanel;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    cxGrid1DBTableView1Column4: TcxGridDBColumn;
    cxGrid2DBTableView1: TcxGridDBTableView;
    cxGrid2Level1: TcxGridLevel;
    cxGrid2: TcxGrid;
    dxBarDockControl1: TdxBarDockControl;
    OrderDatasource: TDataSource;
    cxGrid2DBTableView1Column1: TcxGridDBColumn;
    cxGrid2DBTableView1Column3: TcxGridDBColumn;
    cxGrid2DBTableView1Column4: TcxGridDBColumn;
    dxBarManager1: TdxBarManager;
    dxBarButton1: TdxBarButton;
    dxBarDockControl2: TdxBarDockControl;
    dxBarDockControl3: TdxBarDockControl;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    ActionList1: TActionList;
    actionAddMethology: TAction;
    actionRemoveMethology: TAction;
    dxBarButton4: TdxBarButton;
    MethologyDataset: TClientDataSet;
    MethologyDatasetFormula: TStringField;
    MethologyDatasetKoefP: TStringField;
    MethologyDatasetKoefR: TStringField;
    MethologyDatasetPriceItemID: TStringField;
    MethologyDatasetPriceItemName: TStringField;
    MethologyDatasetPriceItemDescription: TStringField;
    MethologyDatasetIdentificator: TStringField;
    actionSaveMethologyCollection: TAction;
    dxBarButton5: TdxBarButton;
    FileSaveDialog1: TFileSaveDialog;
    actionLoadMethologyCollection: TAction;
    dxBarButton6: TdxBarButton;
    FileOpenDialog1: TFileOpenDialog;
    dxDockPanel4: TdxDockPanel;
    dxBarDockControl4: TdxBarDockControl;
    cxGrid3DBTableView1: TcxGridDBTableView;
    cxGrid3Level1: TcxGridLevel;
    cxGrid3: TcxGrid;
    PriceItemDatasource: TDataSource;
    cxGrid3DBTableView1Column1: TcxGridDBColumn;
    cxGrid3DBTableView1Column2: TcxGridDBColumn;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    actionClose: TAction;
    dxBarSubItem2: TdxBarSubItem;
    dxBarButton9: TdxBarButton;
    dxBarSubItem3: TdxBarSubItem;
    dxBarButton10: TdxBarButton;
    dxBarSubItem4: TdxBarSubItem;
    dxBarButton11: TdxBarButton;
    dxBarSubItem5: TdxBarSubItem;
    dxBarButton12: TdxBarButton;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    actionAddMethology1: TMenuItem;
    actionRemoveMethology1: TMenuItem;
    PopupMenu1: TPopupMenu;
    actionAddMethology2: TMenuItem;
    actionRemoveMethology2: TMenuItem;
    OrderDataset: TClientDataSet;
    OrderDatasetItemPrice: TExtendedField;
    OrderDatasetPredictedPrice: TExtendedField;
    OrderDatasetIdentificator: TStringField;
    OrderDatasetPriceItemName: TStringField;
    dxDockPanel5: TdxDockPanel;
    actionRestartApp: TAction;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    PropertiesDatasource: TDataSource;
    cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    About1: TMenuItem;
    N12: TMenuItem;
    load2: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    dxStatusBar1: TdxStatusBar;
    dxStatusBar2: TdxStatusBar;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxLayoutDockSite4: TdxLayoutDockSite;
    dxLayoutDockSite5: TdxLayoutDockSite;
    actionAddMethologyGroup: TAction;
    procedure actionAddMethologyExecute(Sender: TObject);
    procedure actionAddMethologyGroupExecute(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionLoadMethologyCollectionExecute(Sender: TObject);
    procedure actionRemoveMethologyExecute(Sender: TObject);
    procedure actionRestartAppExecute(Sender: TObject);
    procedure actionSaveMethologyCollectionExecute(Sender: TObject);
    procedure ClientDataSet1CalcFields(DataSet: TDataSet);
    procedure cxDBTreeList1Enter(Sender: TObject);
    procedure cxDBTreeList1Resize(Sender: TObject);
    procedure cxDBVerticalGrid1Edited(Sender: TObject; ARowProperties: TcxCustomEditorRowProperties);
    procedure cxGrid1DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure cxGrid1Enter(Sender: TObject);
    procedure cxGrid1Resize(Sender: TObject);
    procedure cxGrid2DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure cxGrid2Enter(Sender: TObject);
    procedure cxGrid2Resize(Sender: TObject);
    procedure cxGrid3DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure cxGrid3Enter(Sender: TObject);
    procedure cxGrid3Resize(Sender: TObject);
    procedure TreeDatasourceDataChange(Sender: TObject; Field: TField);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure dxBarButton1Click(Sender: TObject);
    procedure MethologyDatasetAfterInsert(DataSet: TDataSet);
    procedure MethologyDatasetAfterPost(DataSet: TDataSet);
    procedure MethologyDatasetBeforeDelete(DataSet: TDataSet);
    procedure PriceItemDatasetAfterInsert(DataSet: TDataSet);
    procedure PriceItemDatasetAfterPost(DataSet: TDataSet);
    procedure TreeDatasetAfterPost(DataSet: TDataSet);
  private
    fOrderList: TList<TOPPPriceOrder>;
    fPriceProject: TOPPPriceProject;
    fPanelTriggerContainer: TOPPHelpVCLPanelTriggerContainer;

    function GetLayoutSettingsFileName: String;

    procedure PerformBuildTree;
    procedure BuildTree;

    procedure FillDatasets;
    procedure FillPriceItemDataset;
    procedure BuildOrderDataset;
    procedure MakeVirtualNode(AMethologyList: TOPPPriceMethologyList);
    function SelectedMethologyList: TOPPPriceMethologyList;
    function SelectedMethology: TOPPPriceMethology;
    procedure ReloadMethologyContent(AMethologyList: TOPPPriceMethologyList);
    procedure InsertMethology(AMethology: TOPPPriceMethology);

    procedure PerformUpdateMethologyList;

    procedure BuildPanelTriggers;

    procedure SetPropertiesDataset(ADataset: TDataSet; APanelCaption: String);

    { Private declarations }
  public
    { Public declarations }
    property LayoutSettingsFileName: String read GetLayoutSettingsFileName;
  end;

var
  OPPPriceCalculator: TOPPPriceCalculator;

implementation

uses
  System.Threading,
  System.IOUtils,
  Winapi.ShellAPI,
  OPP.Help.cxTreeList,
  OPP.Help.cxGrid,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.FormSizeSettings;

type
  StringSplitHelper = record helper for
    String public
    procedure Split(ADelimeter: Char; ListOfStrings: TStrings);
  end;

{$R *.dfm}

procedure TOPPPriceCalculator.actionAddMethologyExecute(Sender: TObject);
var
  fSelectedMethologyList: TOPPPriceMethologyList;
  fGUID: TGUID;
begin
  fSelectedMethologyList := self.SelectedMethologyList;
  if (not Assigned(fSelectedMethologyList)) then
    exit;
  CreateGUID(fGUID);
  fSelectedMethologyList.AddMethology(TOPPPriceMethology.Create(nil, fGUID, ''));
  self.ReloadMethologyContent(fSelectedMethologyList);
end;

procedure TOPPPriceCalculator.actionAddMethologyGroupExecute(Sender: TObject);
begin
//
end;

procedure TOPPPriceCalculator.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPPriceCalculator.actionLoadMethologyCollectionExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    try
      self.fPriceProject := TOPPPriceProject.Load(FileOpenDialog1.Filename);
      FillDatasets;
    except
    end;
  end;
end;

procedure TOPPPriceCalculator.actionRemoveMethologyExecute(Sender: TObject);
begin
  MethologyDataset.DisableControls;
  try
    MethologyDataset.Delete;
  finally
    MethologyDataset.EnableControls;
  end;
end;

procedure TOPPPriceCalculator.actionRestartAppExecute(Sender: TObject);
begin
  if MessageDlg('Сброс настроек требует перезагрузки приложения. Продолжить?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if FileExists(self.LayoutSettingsFileName) then
    begin
      try
        TFile.Delete(self.LayoutSettingsFileName);
      except
      end;

      ShellExecute(Handle, nil, PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
      Application.Terminate;
    end;
  end;
end;

procedure TOPPPriceCalculator.actionSaveMethologyCollectionExecute(Sender: TObject);
begin
  if FileSaveDialog1.Execute then
  begin
    TOPPPriceProject.Save(fPriceProject, FileSaveDialog1.Filename);
  end;
end;

procedure TOPPPriceCalculator.BuildPanelTriggers;
begin
  fPanelTriggerContainer.AddTrigger(dxDockPanel1);
  fPanelTriggerContainer.AddTrigger(dxDockPanel2);
  fPanelTriggerContainer.AddTrigger(dxDockPanel3);
  fPanelTriggerContainer.AddTrigger(dxDockPanel4);
  fPanelTriggerContainer.AddTrigger(dxDockPanel5);
end;

procedure TOPPPriceCalculator.ClientDataSet1CalcFields(DataSet: TDataSet);
var
  fMethologyName, fParentMethologyName: String;
  OutPutList: TStringList;
  i: Integer;
begin
  fMethologyName := DataSet.FieldByName('MethologyName').AsString;

  OutPutList := TStringList.Create;

  try
    fMethologyName.Split('\', OutPutList);

    DataSet.FieldByName('CurrentMethologyName').AsString := OutPutList[OutPutList.Count - 1];
    if OutPutList.Count <= 1 then
    begin
      DataSet.FieldByName('ParentMethologyName').AsString := fMethologyName;
    end else begin
      fParentMethologyName := '';
      for i := 0 to OutPutList.Count - 2 do
      begin
        if length(fParentMethologyName) = 0 then
          fParentMethologyName := OutPutList[i]
        else
          fParentMethologyName := fParentMethologyName + '\' + OutPutList[i];
      end;
      DataSet.FieldByName('ParentMethologyName').AsString := fParentMethologyName;
    end;

  finally
    OutPutList.Free;
  end;
end;

procedure TOPPPriceCalculator.cxDBTreeList1Enter(Sender: TObject);
begin
  SetPropertiesDataset(TreeDataset, 'Свойство группы методик');
end;

procedure TOPPPriceCalculator.cxDBTreeList1Resize(Sender: TObject);
begin
  TOPPHelpCXDBTreeListHelper.SizeColumnToFitTree(cxDBTreeList1, 0);
end;

procedure TOPPPriceCalculator.cxGrid1DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid1, cxGrid1DBTableView1, 1);
end;

procedure TOPPPriceCalculator.cxGrid1Enter(Sender: TObject);
begin
  SetPropertiesDataset(MethologyDataset, 'Свойство методики');
end;

procedure TOPPPriceCalculator.cxGrid1Resize(Sender: TObject);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid1, cxGrid1DBTableView1, 1);
end;

procedure TOPPPriceCalculator.cxGrid2DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid2, cxGrid2DBTableView1, 2);
end;

procedure TOPPPriceCalculator.cxGrid2Enter(Sender: TObject);
begin
  SetPropertiesDataset(OrderDataset, 'Свойство заказа');
end;

procedure TOPPPriceCalculator.cxGrid2Resize(Sender: TObject);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid2, cxGrid2DBTableView1, 2);
end;

procedure TOPPPriceCalculator.cxGrid3DBTableView1ColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid3, cxGrid3DBTableView1, 1);
end;

procedure TOPPPriceCalculator.cxGrid3Enter(Sender: TObject);
begin
  SetPropertiesDataset(PriceItemDataset, 'Свойство статьи затрат');
end;

procedure TOPPPriceCalculator.cxGrid3Resize(Sender: TObject);
begin
  TOPPHelpCXGridHelper.SizeColumnToFitGrid(cxGrid3, cxGrid3DBTableView1, 1);
end;

procedure TOPPPriceCalculator.dxBarButton1Click(Sender: TObject);
begin
  OrderDataset.Append;
  OrderDataset.Post;
end;

procedure TOPPPriceCalculator.TreeDatasourceDataChange(Sender: TObject; Field: TField);
var
  fMethologyList: TOPPPriceMethologyList;
begin
  fMethologyList := SelectedMethologyList;
  ReloadMethologyContent(fMethologyList);
end;

procedure TOPPPriceCalculator.FillDatasets;
var
  fCDS: TClientDataSet;
  fOrderList: TList<TOPPPriceOrder>;
begin

  FillPriceItemDataset;

  PerformBuildTree;

  TTask.Run(
    procedure()
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          Application.ProcessMessages;

          BuildOrderDataset;
        end);
    end);
end;

procedure TOPPPriceCalculator.BuildOrderDataset;
var
  fOrder: TOPPPriceOrder;
  fItem: TOPPPriceOrderItem;
begin
  OrderDataset.DisableControls;
  try
    OrderDataset.EmptyDataSet;

    fOrderList := TOPPPriceOrderDefaults.GetDefaultList;
    for fOrder in fOrderList do
    begin
      for fItem in fOrder.ItemList do
      begin
        OrderDataset.Insert;
        OrderDataset.FieldByName('Identificator').AsString := fItem.Identificator;
        OrderDataset.FieldByName('PriceItemName').AsString := fItem.PriceItem.Name;
        OrderDataset.FieldByName('ItemPrice').AsExtended := fItem.Summ;
        OrderDataset.Post;
      end;
    end;
  finally
    OrderDataset.EnableControls;
    cxGrid2DBTableView1.DataController.UpdateData;
  end;
end;

procedure TOPPPriceCalculator.FillPriceItemDataset;
var
  fPriceItem: TOPPPriceItem;

begin
  PriceItemDataset.DisableControls;
  try
    PriceItemDataset.AfterInsert := nil;
    PriceItemDataset.AfterPost := nil;

    PriceItemDataset.EmptyDataSet;

    for fPriceItem in fPriceProject.PriceItems do
    begin
      PriceItemDataset.Append;
      PriceItemDataset.FieldByName('PriceItemID').AsString := fPriceItem.Identifier;
      PriceItemDataset.FieldByName('PriceItemName').AsString := fPriceItem.Name;
      PriceItemDataset.FieldByName('PriceItemDescription').AsString := fPriceItem.Description;
      PriceItemDataset.Post;
    end;
  finally
    PriceItemDataset.AfterPost := PriceItemDatasetAfterPost;
    PriceItemDataset.AfterInsert := PriceItemDatasetAfterInsert;
    PriceItemDataset.EnableControls;
  end;

end;

procedure TOPPPriceCalculator.BuildTree;
var
  fMethologyList: TOPPPriceMethologyList;

begin
  TreeDataset.DisableControls;
  try
    TreeDataset.DisableConstraints;

    TreeDataset.AfterPost := nil;
    try
      TreeDataset.EmptyDataSet;

      for fMethologyList in fPriceProject.MethologyCollection do
      begin
        MakeVirtualNode(fMethologyList);
      end;
    finally

      TreeDataset.AfterPost := TreeDatasetAfterPost;
      TreeDataset.EnableConstraints;
    end;
  finally
    TreeDataset.EnableControls;
  end;
end;

procedure TOPPPriceCalculator.cxDBVerticalGrid1Edited(Sender: TObject; ARowProperties: TcxCustomEditorRowProperties);
begin
  cxDBVerticalGrid1.DataController.Post();
end;

procedure TOPPPriceCalculator.MakeVirtualNode(AMethologyList: TOPPPriceMethologyList);
var
  fCDS: TClientDataSet;
  fList: TStringList;
  fNode, fNodeName, fParentNodeName: String;
begin

  fList := TStringList.Create;
  try
    AMethologyList.Name.Split('\', fList);
    fCDS := TClientDataSet.Create(nil);
    try
      fCDS.CloneCursor(TreeDataset, false);
      fNodeName := '';
      fParentNodeName := '';
      for fNode in fList do
      begin

        if length(fNodeName) = 0 then
          fNodeName := fNode
        else
          fNodeName := Format('%s\%s', [fNodeName, fNode]);

        fCDS.IndexFieldNames := 'MethologyName';
        if not fCDS.Locate('MethologyName', fNodeName, [loCaseInsensitive]) then
        begin
          TreeDataset.Append;
          TreeDataset.FieldByName('MethologyName').AsString := fNodeName;
          TreeDataset.FieldByName('ParentMethologyName').AsString := fParentNodeName;
          if CompareStr(fNodeName, AMethologyList.Name) = 0 then
          begin
            TreeDataset.FieldByName('Identificator').AsString := AMethologyList.Identificator;
          end;
          TreeDataset.Post;
        end;

        if length(fParentNodeName) = 0 then
          fParentNodeName := fNode
        else
          fParentNodeName := Format('%s\%s', [fParentNodeName, fNode]);
      end;
    finally
      fCDS.Free;
    end;
  finally
    fList.Free;
  end;
end;

function TOPPPriceCalculator.GetLayoutSettingsFileName: String;
var
  fFileName: String;
begin
  fFileName := Format('%s.%s', [self.ClassName, 'Layout.settings']);
  result := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(fFileName);
end;

procedure TOPPPriceCalculator.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fOrderList.Clear;
  fOrderList.Free;
  fPriceProject.Free;
  dxDockingManager1.SaveLayoutToIniFile(self.LayoutSettingsFileName);
  self.SaveFormState;
end;

procedure TOPPPriceCalculator.FormCreate(Sender: TObject);
var
  fMethologyList: TOPPPriceMethologyList;
  fMethodName: String;
  fGUID1, fGUID2, fGUID3, fGUID4: TGUID;
  fGUID5, fGUID6: TGUID;
begin

  self.ReadFormState;
  dxDockingManager1.LoadLayoutFromIniFile(self.LayoutSettingsFileName);

  fPanelTriggerContainer := TOPPHelpVCLPanelTriggerContainer.Create(N9);
  BuildPanelTriggers();

  fPriceProject := TOPPPriceProject.Create;

  CreateGUID(fGUID1);
  CreateGUID(fGUID2);
  CreateGUID(fGUID3);
  CreateGUID(fGUID4);
  CreateGUID(fGUID5);
  CreateGUID(fGUID6);

  fOrderList := TList<TOPPPriceOrder>.Create;

  fPriceProject.PriceItems.AddRange(TOPPPriceItemDefaults.All);

  fMethologyList := TOPPPriceMethologyList.Create('Группы методик расчёта\Внутризаводские методики\Внутризаводской заказ(июнь 2002)', fGUID5);
  fMethologyList.AddMethology(TOPPPriceMethology.Create(TOPPPriceItemDefaults.Material, fGUID1, '{ПКИ}*0.2'));
  fMethologyList.AddMethology(TOPPPriceMethology.Create(TOPPPriceItemDefaults.PKI, fGUID2, '{СЦ}*0.3'));
  fPriceProject.MethologyCollection.Add(fMethologyList);

  fMethologyList := TOPPPriceMethologyList.Create('Группы методик расчёта\Внутризаводские методики\Внутризаводской заказ(август 2002)', fGUID6);
  fMethologyList.AddMethology(TOPPPriceMethology.Create(TOPPPriceItemDefaults.SellPrice, fGUID3, '{ПКИ}*0.1'));
  fMethologyList.AddMethology(TOPPPriceMethology.Create(TOPPPriceItemDefaults.PKI, fGUID4, '{СЦ}*0.4'));
  fPriceProject.MethologyCollection.Add(fMethologyList);

  FillDatasets;

end;

procedure TOPPPriceCalculator.InsertMethology(AMethology: TOPPPriceMethology);
begin

  MethologyDataset.AfterInsert := nil;

  MethologyDataset.Insert;
  if Assigned(AMethology) then
  begin
    if Assigned(AMethology.PriceItem) then
    begin
      MethologyDataset.FieldByName('PriceItemID').AsString := AMethology.PriceItem.Identifier;
    end;
    MethologyDataset.FieldByName('Identificator').AsString := AMethology.Identificator;
    MethologyDataset.FieldByName('Formula').AsString := AMethology.Formula;
    MethologyDataset.FieldByName('KoefP').AsString := AMethology.KoefP;
    MethologyDataset.FieldByName('KoefR').AsString := AMethology.KoefR;
  end;
  MethologyDataset.Post;

  MethologyDataset.AfterInsert := MethologyDatasetAfterInsert;
end;

procedure TOPPPriceCalculator.MethologyDatasetAfterInsert(DataSet: TDataSet);
var
  fMethologyList: TOPPPriceMethologyList;
  fMethology: TOPPPriceMethology;
  fGUID: TGUID;
begin

  fMethologyList := self.SelectedMethologyList;
  if not Assigned(fMethologyList) then
    exit;

  CreateGUID(fGUID);
  MethologyDataset.FieldByName('Identificator').AsString := GUIDToString(fGUID);

  fMethology := TOPPPriceMethology.Create(nil, fGUID, '');
  fMethologyList.AddMethology(fMethology);
end;

procedure TOPPPriceCalculator.MethologyDatasetAfterPost(DataSet: TDataSet);
var
  fSelectedMethology: TOPPPriceMethology;
  fPriceItem: TOPPPriceItem;
begin
  fSelectedMethology := SelectedMethology;
  if not Assigned(fSelectedMethology) then
    exit;

  for fPriceItem in fPriceProject.PriceItems do
  begin
    if fPriceItem.Identifier = DataSet.FieldByName('PriceItemID').AsString then
    begin
      fSelectedMethology.PriceItem := fPriceItem;
    end;
  end;

  fSelectedMethology.Formula := DataSet.FieldByName('Formula').AsString;
  fSelectedMethology.KoefP := DataSet.FieldByName('KoefP').AsString;
  fSelectedMethology.KoefR := DataSet.FieldByName('KoefR').AsString;
end;

procedure TOPPPriceCalculator.MethologyDatasetBeforeDelete(DataSet: TDataSet);
var
  fSelectedMethologyList: TOPPPriceMethologyList;
  fID: String;
begin
  fSelectedMethologyList := self.SelectedMethologyList;
  if (not Assigned(fSelectedMethologyList)) then
    exit;
  fID := MethologyDataset.FieldByName('Identificator').AsString;
  fSelectedMethologyList.RemoveMethology(fID);
  //
end;

procedure TOPPPriceCalculator.PerformBuildTree;
begin
  TTask.Run(
    procedure()
    begin
      TThread.Synchronize(nil,
        procedure()
        begin
          self.BuildTree;
        end);
    end);
end;

procedure TOPPPriceCalculator.PerformUpdateMethologyList;
begin

end;

procedure TOPPPriceCalculator.PriceItemDatasetAfterInsert(DataSet: TDataSet);
var
  fGUID: TGUID;
  fPriceItem: TOPPPriceItem;
begin
  if DataSet.FieldByName('PriceItemID').IsNull then
  begin
    CreateGUID(fGUID);
    DataSet.FieldByName('PriceItemID').AsString := GUIDToString(fGUID);

    fPriceItem := TOPPPriceItem.Create(DataSet.FieldByName('PriceItemName').AsString, fGUID, DataSet.FieldByName('PriceItemDescription').AsString);
    fPriceProject.PriceItems.Add(fPriceItem);
  end;
end;

procedure TOPPPriceCalculator.PriceItemDatasetAfterPost(DataSet: TDataSet);
var
  fPriceItem: TOPPPriceItem;
begin
  for fPriceItem in fPriceProject.PriceItems do
  begin
    if fPriceItem.Identifier = PriceItemDataset.FieldByName('PriceItemID').AsString then
    begin
      fPriceItem.Name := PriceItemDataset.FieldByName('PriceItemName').AsString;
      fPriceItem.Description := PriceItemDataset.FieldByName('PriceItemDescription').AsString;
    end;
  end;
end;

procedure TOPPPriceCalculator.ReloadMethologyContent(AMethologyList: TOPPPriceMethologyList);
var
  fml: TOPPPriceMethologyList;
  fMethology: TOPPPriceMethology;
begin

  if (not MethologyDataset.Active) then
    exit;

  MethologyDataset.DisableControls;
  try
    MethologyDataset.AfterPost := nil;

    MethologyDataset.EmptyDataSet;
    if Assigned(AMethologyList) then
    begin
      for fMethology in AMethologyList.List do
      begin
        InsertMethology(fMethology);
      end;
    end;
  finally

    MethologyDataset.AfterPost := MethologyDatasetAfterPost;
    MethologyDataset.EnableControls;
  end;
end;

function TOPPPriceCalculator.SelectedMethology: TOPPPriceMethology;
var
  fMethologyList: TOPPPriceMethologyList;
  fMethology: TOPPPriceMethology;
  fID: String;
begin

  result := nil;

  fMethologyList := SelectedMethologyList;
  if (not Assigned(fMethologyList)) then
    exit;

  fID := MethologyDataset.FieldByName('Identificator').AsString;
  for fMethology in fMethologyList.List do
  begin
    if fMethology.Identificator = fID then
    begin
      result := fMethology;
    end;
  end;
end;

function TOPPPriceCalculator.SelectedMethologyList: TOPPPriceMethologyList;
var
  fml: TOPPPriceMethologyList;
begin

  result := nil;
  if (not Assigned(fPriceProject)) then
    exit;

  for fml in fPriceProject.MethologyCollection do
  begin
    if (fml.Identificator = TreeDatasource.DataSet.FieldByName('Identificator').AsString) then
      result := fml;
  end;
end;

procedure TOPPPriceCalculator.SetPropertiesDataset(ADataset: TDataSet; APanelCaption: String);
var
  fField: TField;
  i: Integer;
  fRow: TcxCustomRow;
  fDBRow: TcxDBEditorRow;
begin

  dxDockPanel5.Caption := APanelCaption;

  for i := cxDBVerticalGrid1.Rows.Count - 1 downto 0 do
  begin
    fRow := cxDBVerticalGrid1.Rows.Items[i];
    cxDBVerticalGrid1.Remove(fRow);
  end;

  PropertiesDatasource.DataSet := ADataset;
  for fField in ADataset.Fields do
  begin
    fRow := cxDBVerticalGrid1.Add(TcxDBEditorRow);
    fDBRow := (fRow as TcxDBEditorRow);
    if Assigned(fDBRow) then
      fDBRow.Properties.DataBinding.FieldName := fField.FieldName;
  end;
end;

procedure TOPPPriceCalculator.TreeDatasetAfterPost(DataSet: TDataSet);
var
  fSelectedMethologyList: TOPPPriceMethologyList;
begin

  fSelectedMethologyList := self.SelectedMethologyList;
  if (not Assigned(fSelectedMethologyList)) then
    exit;
  fSelectedMethologyList.Name := DataSet.FieldByName('MethologyName').AsString;

  PerformBuildTree;
end;

{ StringSplitHelper }

procedure StringSplitHelper.Split(ADelimeter: Char; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := ADelimeter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := self;
end;

end.
