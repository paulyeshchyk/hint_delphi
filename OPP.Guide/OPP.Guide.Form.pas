unit OPP.Guide.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxPC, dxDockControl, cxControls, dxDockPanel, cxClasses, Vcl.ComCtrls, dxtree,
  Data.DB, Datasnap.DBClient, dxdbtree, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxMaskEdit, cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, cxInplaceContainer, cxDBTL,
  cxTLData, cxEdit, cxVGrid, cxDBVGrid, dxBar, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, cxContainer,
  cxTextEdit, cxMemo, cxDBLookupComboBox, cxBlobEdit, cxDropDownEdit, cxDBEdit, dxStatusBar,

  OPP_Guide_API,
  OPP_Guide_API_Dataprovider,
  OPP.Guide.executor,
  OPP_Guide_Executor,
  OPP_Guide_Executor_State,

  OPP.Help.Vcl.PanelTrigger,
  OPP.Guide.Settings,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Guide.Scripter, ScrMemo, ScrMps, Vcl.Menus;

type
  TOPPGuideForm = class(TForm)
    actionAddChildRecord: TAction;
    actionAddChildRecord1: TMenuItem;
    actionAddChildRecord2: TMenuItem;
    actionAddRecord: TAction;
    actionAddRecord1: TMenuItem;
    actionAddRecord2: TMenuItem;
    actionClearRecentList: TAction;
    actionClose: TAction;
    actionClose1: TMenuItem;
    actionCompileScript1: TMenuItem;
    actionCompileScript2: TMenuItem;
    actionExport1: TMenuItem;
    actionFindPanelShow: TAction;
    actionGuideExport: TAction;
    actionGuideExportAs: TAction;
    actionGuideNew: TAction;
    actionGuideOpen: TAction;
    actionGuideReload: TAction;
    actionGuideRunAll: TAction;
    actionGuideRunSelected: TAction;
    actionHelp: TAction;
    actionHelp1: TMenuItem;
    ActionList1: TActionList;
    actionNew1: TMenuItem;
    actionOpen1: TMenuItem;
    actionRemoveRecord: TAction;
    actionRemoveRecord1: TMenuItem;
    actionRemoveRecord2: TMenuItem;
    actionRunAll1: TMenuItem;
    actionRunScript1: TMenuItem;
    actionRunSelected1: TMenuItem;
    actionSave1: TMenuItem;
    actionScriptCompile: TAction;
    actionScriptRun: TAction;
    actionScriptSave: TAction;
    cxDBTreeList1: TcxDBTreeList;
    cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn;
    cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow6: TcxDBEditorRow;
    cxMemo1: TcxMemo;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyleRepository1: TcxStyleRepository;
    DataSetNodeType: TClientDataSet;
    DataSetNodeTypecaption: TWideStringField;
    DataSetNodeTypeid: TIntegerField;
    DataSetTreeView: TClientDataSet;
    DataSetTreeViewActionText: TStringField;
    DataSetTreeViewCaption: TWideStringField;
    DataSetTreeViewIdentifier: TStringField;
    DataSetTreeViewNodeType: TIntegerField;
    DataSetTreeViewOrder: TIntegerField;
    DataSetTreeViewPIdentifier: TStringField;
    DataSetTreeViewScript: TBlobField;
    DataSourceNodeType: TDataSource;
    DataSourceTreeView: TDataSource;
    dxBarButton1: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarButton12: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarDockControl2: TdxBarDockControl;
    dxBarDockControl3: TdxBarDockControl;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar2: TdxBar;
    dxBarSubItem1: TdxBarSubItem;
    dxDockingManager1: TdxDockingManager;
    dxDockPanel5: TdxDockPanel;
    dxDockPanel6: TdxDockPanel;
    dxDockPanelOutputLog: TdxDockPanel;
    dxDockPanelProperties: TdxDockPanel;
    dxDockPanelScript: TdxDockPanel;
    dxDockPanelTreeView: TdxDockPanel;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxLayoutDockSite3: TdxLayoutDockSite;
    dxStatusBar1: TdxStatusBar;
    dxStatusBar2: TdxStatusBar;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    FileOpenDialog1: TFileOpenDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
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
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrMemo1: TScrMemo;
    ScrMemoSource1: TScrMemoSource;
    ScrPascalMemoStyler1: TScrPascalMemoStyler;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure actionClearRecentListExecute(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionFindPanelShowExecute(Sender: TObject);
    procedure actionGuideExportAsExecute(Sender: TObject);
    procedure actionGuideExportExecute(Sender: TObject);
    procedure actionGuideNewExecute(Sender: TObject);
    procedure actionGuideOpenExecute(Sender: TObject);
    procedure actionGuideReloadExecute(Sender: TObject);
    procedure actionGuideRunAllExecute(Sender: TObject);
    procedure actionGuideRunSelectedExecute(Sender: TObject);
    procedure actionHelpExecute(Sender: TObject);
    procedure actionRemoveRecordExecute(Sender: TObject);
    procedure actionScriptCompileExecute(Sender: TObject);
    procedure actionScriptRunExecute(Sender: TObject);
    procedure actionScriptSaveExecute(Sender: TObject);
    procedure cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cxDBTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxDBVerticalGrid1DBEditorRow6EditPropertiesEditValueChanged(Sender: TObject);
    procedure DataSetTreeViewAfterApplyUpdates(Sender: TObject; var OwnerData: OLEVariant);
    procedure DataSetTreeViewAfterOpen(DataSet: TDataSet);
    procedure DataSetTreeViewAfterPost(DataSet: TDataSet);
    procedure DataSetTreeViewAfterScroll(DataSet: TDataSet);
    procedure DataSetTreeViewBeforeEdit(DataSet: TDataSet);
    procedure dxStatusBar1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ScrMemo1Change(Sender: TObject);
    procedure ScrMemo1CursorChange(Sender: TObject);
  private
    fDataprovider: TOPPGuideAPIDataprovider;
    fFilename: String;
    fPanelTriggerContainer: TOPPHelpVCLPanelTriggerContainer;
    fSaveActionIsInProgress: Boolean;
    fScripter: IOPPGuideScripter;
    fScriptPanelTrigger: TOPPHelpVCLPanelTrigger;
    { Private declarations }
    fSettings: IOPPGuideSettings;
    procedure BuildPanelTriggers(AMenuItem: TMenuItem);
    procedure DoUpdateStatusBarWidths;
    function fIdentifiableClone(ADataset: TClientDataSet): IOPPGuideAPIIdentifiable;
    procedure fScriptRunCompletion(AState: TOPPGuideExecutorRunState);
    class function GetApplicationTitle: String; static;
    procedure LoadDatasetContent(AFilename: String);
    procedure ReadDefaults;
    procedure SetFilename(const Value: String);
    property Filename: String read fFilename write SetFilename;
  public
    class property ApplicationTitle: String read GetApplicationTitle;
    { Public declarations }
    property SaveActionIsInProgress: Boolean read fSaveActionIsInProgress write fSaveActionIsInProgress default false;
  end;

var
  OPPGuideForm: TOPPGuideForm;

const
  kContext: String = 'GuideForm';

resourcestring
  SDefaultCaption = 'ГОЛЬФСТРИМ: Редактор сценариев';

implementation

uses
  OPP.Help.Log,
  OPP.Help.System.Str,
  OPP.Help.System.Files,
  OPP.Help.System.Application,

  OPP.Guide.Scripter.TMS,
  OPP.Guide.Scripter.DC,

  OPP_Guide_API_Context_Step_Wrapper,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Context,

  midaslib;

type
  TOPPTreeDatasetHelpler = class helper for TClientDataSet
    procedure swapValues(AFieldName: String; AIDFieldName: String; sourceId, destinationId: Variant);
  end;

  TForm1Helper = class helper for TOPPGuideForm
    function ScriptPanelIsVisible: Boolean;
  end;

  TcxTreeListNodeHelper = class helper for TcxTreeListNode
    procedure DeleteAllDescendants;
  end;

{$R *.dfm}

procedure TOPPGuideForm.actionAddChildRecordExecute(Sender: TObject);
var
  pid: Variant;
  fActiveItem: TOPPGuideAPIContextStep;
begin

  fActiveItem := TOPPGuideAPIContextStep(fDataprovider.ActiveItem);
  if not assigned(fActiveItem) then
    exit;

  cxDBTreeList1.BeginUpdate;
  try
    try
      fDataprovider.AddChild(fActiveItem.IdentifierValue);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    cxDBTreeList1.EndUpdate;
  end;
end;

procedure TOPPGuideForm.actionAddRecordExecute(Sender: TObject);
begin
  cxDBTreeList1.BeginUpdate;
  try
    try
      fDataprovider.Add;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    cxDBTreeList1.EndUpdate;
  end;
end;

procedure TOPPGuideForm.actionClearRecentListExecute(Sender: TObject);
begin
  fSettings.ClearHierarchyFilenameRecentList;
end;

procedure TOPPGuideForm.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPGuideForm.actionFindPanelShowExecute(Sender: TObject);
begin
  cxDBTreeList1.SetFocus;
end;

procedure TOPPGuideForm.actionGuideExportAsExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
    DataSetTreeView.SaveToFile(SaveDialog1.Filename, dfXMLUTF8);
    self.Filename := SaveDialog1.Filename;
  end;
end;

procedure TOPPGuideForm.actionGuideExportExecute(Sender: TObject);
begin
  DataSetTreeView.SaveToFile(self.Filename, dfXMLUTF8);
end;

procedure TOPPGuideForm.actionGuideNewExecute(Sender: TObject);
begin
  DataSetTreeView.DisableControls;
  try
    DataSetTreeView.EmptyDataSet;
    self.Filename := '';
  finally
    DataSetTreeView.EnableControls;
  end;
end;

procedure TOPPGuideForm.actionGuideOpenExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    LoadDatasetContent(FileOpenDialog1.Filename);
  end;
end;

procedure TOPPGuideForm.actionGuideReloadExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    LoadDatasetContent(FileOpenDialog1.Filename);
  end;
end;

procedure TOPPGuideForm.actionGuideRunAllExecute(Sender: TObject);
var
  fFilter: String;
  cloned: TClientDataSet;
  fObject: IOPPGuideAPIIdentifiable;
begin

  fFilter := Format('pidentifier IS NULL', []);

  cxMemo1.Clear;

  cloned := TClientDataSet.Create(nil);
  try
    cloned.CloneCursor(DataSetTreeView, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    cloned.First;
    while not cloned.Eof do
    begin
      fObject := fIdentifiableClone(cloned);

      try
        TOPPGuideExecutor.shared.run(DataSetTreeView, fObject, fIdentifiableClone, true, fScripter, fScriptRunCompletion);
      except
        on E: Exception do
        begin
          eventLogger.Error(E, kContext);
        end;
      end;

      cloned.Next;
    end;
  finally
    cloned.Free;
  end;

end;

procedure TOPPGuideForm.actionGuideRunSelectedExecute(Sender: TObject);
var
  fScriptResult: Variant;
  fObject: IOPPGuideAPIIdentifiable;
begin
  cxMemo1.Clear;

  fObject := fIdentifiableClone(DataSetTreeView);
  try
    try
      TOPPGuideExecutor.shared.run(DataSetTreeView, fObject, fIdentifiableClone, true, fScripter, fScriptRunCompletion);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    fObject := nil;
  end;
end;

procedure TOPPGuideForm.actionHelpExecute(Sender: TObject);
begin
  Application.openMarketingWebPage;
end;

procedure TOPPGuideForm.actionRemoveRecordExecute(Sender: TObject);
begin
  cxDBTreeList1.BeginUpdate;
  cxDBTreeList1.FocusedNode.DeleteAllDescendants;
  cxDBTreeList1.FocusedNode.Delete;
  cxDBTreeList1.EndUpdate;
end;

procedure TOPPGuideForm.actionScriptCompileExecute(Sender: TObject);
begin
  actionScriptSave.Execute;

  TOPPGuideExecutor.shared.compile(DataSetTreeView,
    function(ADataset: TClientDataSet): IOPPGuideAPIIdentifiable
    begin
    end, false, fScripter,self.fScriptRunCompletion);
end;

procedure TOPPGuideForm.actionScriptRunExecute(Sender: TObject);
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  cxMemo1.Clear;

  try
    fObject := fIdentifiableClone(DataSetTreeView);
    try
      TOPPGuideExecutor.shared.run(DataSetTreeView, fObject, fIdentifiableClone, false, fScripter, fScriptRunCompletion);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    fObject := nil;
  end;
end;

procedure TOPPGuideForm.actionScriptSaveExecute(Sender: TObject);
begin
  self.SaveActionIsInProgress := true;
  try
    DataSourceTreeView.DataSet.DisableControls;
    if DataSourceTreeView.DataSet.State in [dsEdit, dsInsert, dsNewValue] then
      DataSourceTreeView.DataSet.Post;
    ScrMemoSource1.Modified := false;
    dxStatusBar1.Panels[2].text := 'Insert';
    DoUpdateStatusBarWidths;
    DataSourceTreeView.DataSet.EnableControls;
  finally
    self.SaveActionIsInProgress := false;
  end;
end;

procedure TOPPGuideForm.cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //
end;

procedure TOPPGuideForm.cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  OutputDebugString(Format('%d', [Integer(State)]).toWideChar);
end;

procedure TOPPGuideForm.cxDBTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
var
  fActiveItemSubscCount: Integer;
begin
  fActiveItemSubscCount := fDataprovider.ActiveItemSubscCount;

  actionGuideRunAll.Enabled := (fActiveItemSubscCount > 0);
  if assigned(fScriptPanelTrigger) then
  begin
    fScriptPanelTrigger.Enabled := false;
    dxDockPanelScript.Visible := ScriptPanelIsVisible;
    fScriptPanelTrigger.Enabled := ScriptPanelIsVisible;
  end else begin
    dxDockPanelScript.Visible := ScriptPanelIsVisible;
  end;
end;

procedure TOPPGuideForm.cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
var
  fGUID: TGuid;
begin
  CreateGUID(fGUID);
  AFocusedNode.KeyValue := GUIDToString(fGUID);
  AHandled := true;
end;

procedure TOPPGuideForm.cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  node, nodeToReplace: TcxTreeListNode;
  kv1, kv2: Variant;
begin
  if cxDBTreeList1.IsEditing then
    exit;
  if Shift <> [ssCtrl] then
    exit;

  node := cxDBTreeList1.FocusedNode;
  if not assigned(node) or not(node is TcxDBTreeListNode) then
    exit;
  kv2 := TcxDBTreeListNode(node).KeyValue;

  case Key of
    VK_UP:
      begin
        nodeToReplace := node.GetPrevSiblingVisible;
      end;
    VK_DOWN:
      begin
        nodeToReplace := node.GetNextSiblingVisible;
      end;
  end;

  if assigned(nodeToReplace) and (nodeToReplace is TcxDBTreeListNode) then
  begin
    kv1 := TcxDBTreeListNode(nodeToReplace).KeyValue;
    DataSetTreeView.swapValues('Order', 'identifier', kv1, kv2);
    Key := 0; // VK_ESCAPE;
  end;
end;

procedure TOPPGuideForm.cxDBVerticalGrid1DBEditorRow6EditPropertiesEditValueChanged(Sender: TObject);
begin
  dxDockPanelScript.Visible := ScriptPanelIsVisible();
end;

procedure TOPPGuideForm.DataSetTreeViewAfterApplyUpdates(Sender: TObject; var OwnerData: OLEVariant);
begin
  //
end;

procedure TOPPGuideForm.DataSetTreeViewAfterOpen(DataSet: TDataSet);
begin
  cxDBTreeList1cxDBTreeListColumn1.Width := cxDBTreeList1.Width;
end;

procedure TOPPGuideForm.DataSetTreeViewAfterPost(DataSet: TDataSet);
begin
  actionScriptCompile.Enabled := false;
end;

procedure TOPPGuideForm.DataSetTreeViewAfterScroll(DataSet: TDataSet);
var
  fStream: TStream;
begin
  actionAddChildRecord.Enabled := (DataSourceTreeView.DataSet.RecordCount <> 0) and (not DataSourceTreeView.DataSet.FieldByName('identifier').IsNull);
  actionRemoveRecord.Enabled := assigned(cxDBTreeList1.FocusedNode);

  if not self.SaveActionIsInProgress then
  begin
    fStream := DataSetTreeView.CreateBlobStream(DataSetTreeView.FieldByName('Script'), bmRead);
    try
      ScrMemoSource1.Lines.LoadFromStream(fStream);
    finally
      fStream.Free;
    end;
  end;
end;

procedure TOPPGuideForm.DataSetTreeViewBeforeEdit(DataSet: TDataSet);
begin
  actionScriptCompile.Enabled := true;
end;

procedure TOPPGuideForm.dxStatusBar1Resize(Sender: TObject);
begin
  DoUpdateStatusBarWidths;
end;

procedure TOPPGuideForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

{$REGION '  To be removed asap'}
  TOPPGuideAPIContext.shared.SetDataprovider(nil);
{$ENDREGION}
  fScripter := nil;
  fScriptPanelTrigger := nil;
  fSettings := nil;
end;

procedure TOPPGuideForm.FormCreate(Sender: TObject);
begin

  eventLogger.Flow('Form Created', kContext);

{$REGION '  To be removed asap'}
  fDataprovider := TOPPGuideAPIDataprovider.Create(self);
  fDataprovider.ClientDataset := self.DataSetTreeView;
  TOPPGuideAPIContext.shared.SetDataprovider(fDataprovider);
{$ENDREGION}
  actionHelp.Caption := Application.BuildNumber;
  self.Caption := TOPPGuideForm.GetApplicationTitle;

  fScripter := TOPPGuideScripterDC.Create;

  fSettings := TOPPGuideSettings.Create;

  ReadDefaults;

  BuildPanelTriggers(N2);

  DoUpdateStatusBarWidths;

end;

procedure TOPPGuideForm.ScrMemo1Change(Sender: TObject);
var
  fStream: TStream;
begin
  if self.SaveActionIsInProgress then
    exit;

  if not ScrMemoSource1.Modified then
    exit;

  dxStatusBar1.Panels[2].text := 'Modified';
  DoUpdateStatusBarWidths;

  if not DataSetTreeView.Modified then
    DataSetTreeView.Edit;

  fStream := DataSetTreeView.CreateBlobStream(DataSetTreeView.FieldByName('Script'), bmWrite);
  try
    ScrMemo1.Lines.SaveToStream(fStream);
  finally
    fStream.Free;
  end;
end;

procedure TOPPGuideForm.ScrMemo1CursorChange(Sender: TObject);
begin
  dxStatusBar2.Panels[0].text := Format('%4.d:%d', [ScrMemo1.CurY + 1, ScrMemo1.CurX + 1]);
end;

procedure TOPPGuideForm.BuildPanelTriggers(AMenuItem: TMenuItem);
begin
  fPanelTriggerContainer := TOPPHelpVCLPanelTriggerContainer.Create(AMenuItem);
  fPanelTriggerContainer.AddTrigger(dxDockPanelTreeView);
  fPanelTriggerContainer.AddTrigger(dxDockPanelProperties);
  fScriptPanelTrigger := fPanelTriggerContainer.AddTrigger(dxDockPanelScript);
  fPanelTriggerContainer.AddTrigger(dxDockPanelOutputLog);
  fPanelTriggerContainer.AddTrigger(dxDockPanel5);
  fPanelTriggerContainer.AddTrigger(dxDockPanel6);
end;

procedure TOPPGuideForm.DoUpdateStatusBarWidths;
var
  l1, l2: Integer;
begin
  l1 := 40 + dxStatusBar1.Canvas.TextWidth(dxStatusBar1.Panels[0].text);
  l2 := 40 + dxStatusBar1.Canvas.TextWidth(dxStatusBar1.Panels[2].text);
  dxStatusBar1.Panels[0].Width := l1;
  dxStatusBar1.Panels[1].Width := dxStatusBar1.Width - l1 - l2;
  dxStatusBar1.Panels[2].Width := l2;

  dxStatusBar2.Panels[0].Width := 40 + dxStatusBar2.Canvas.TextWidth('9999:9999');
end;

function TOPPGuideForm.fIdentifiableClone(ADataset: TClientDataSet): IOPPGuideAPIIdentifiable;
begin
  result := nil;
  if not assigned(ADataset) then
    exit;
  result := TOPPGuideAPIContextStep.WrapFromDataset1(ADataset);
end;

procedure TOPPGuideForm.fScriptRunCompletion(AState: TOPPGuideExecutorRunState);
begin
  cxMemo1.Lines.Add(AState.shortDescription);
  eventLogger.Flow(AState.shortDescription, 'GuideAPI');
end;

class function TOPPGuideForm.GetApplicationTitle: String;
begin
  result := SDefaultCaption;
end;

procedure TOPPGuideForm.LoadDatasetContent(AFilename: String);
begin
  if TOPPHelpSystemFilesHelper.FilenameHasPath(AFilename) then
    self.Filename := AFilename
  else
    self.Filename := TOPPHelpSystemFilesHelper.GetOPPGuidePath(AFilename);

  fSettings.SetDefaultHierarchyFilename(self.Filename);

  if FileExists(self.Filename) then
  begin
    cxDBTreeList1.BeginUpdate;
    DataSetTreeView.LoadFromFile(self.Filename);
    cxDBTreeList1.EndUpdate;
  end else begin
    eventLogger.Error(Format('File not found: %s', [self.Filename]), kContext);
  end;
end;

procedure TOPPGuideForm.ReadDefaults;
begin
  fSettings.OnFormFrameLoad(
    procedure(AFrame: TRect)
    begin
      self.Frame := AFrame;
    end);
  fSettings.OnFormFrameSave(
    function: TRect
    begin
      result := self.Frame;
    end);
  fSettings.OnDockingFileLoad(
    procedure(ADockingFileName: String)
    var
      fIniFile: String;
    begin
      fIniFile := TOPPHelpSystemFilesHelper.GetOPPGuidePath(ADockingFileName);
      if FileExists(fIniFile) then
        dxDockingManager1.LoadLayoutFromIniFile(fIniFile);
    end);
  fSettings.OnDockingFilenameGet(
    function: String
    begin
      result := 'opp.guide.docking.ini';
    end);
  fSettings.OnDockingFileSave(
    procedure(AFilename: String)
    var
      fIniFile: String;
    begin
      fIniFile := TOPPHelpSystemFilesHelper.GetOPPGuidePath(AFilename);
      dxDockingManager1.SaveLayoutToIniFile(fIniFile);
    end);
  fSettings.OnHierarchyFileLoad(
    procedure(AFilename: String)
    begin
      LoadDatasetContent(AFilename);
    end);
  fSettings.OnHierarchyFilenameGet(
    function: String
    begin
      result := 'opp.guide.xml';
    end);
  fSettings.OnHierarchyFileSave(
    procedure(AFilename: String)
    var
      fDatasetXMLFile: String;
    begin
      if TOPPHelpSystemFilesHelper.FilenameHasPath(AFilename) then
        fDatasetXMLFile := AFilename
      else
        fDatasetXMLFile := TOPPHelpSystemFilesHelper.GetOPPGuidePath(AFilename);
      DataSetTreeView.SaveToFile(fDatasetXMLFile, dfXMLUTF8);
    end);
  fSettings.OnHierarchyFilenameRecentListLoad(
    procedure(AList: TStringlist)
    var
      newItem: TMenuItem;
      item: String;
    begin
      for item in AList do
      begin
        newItem := TMenuItem.Create(MainMenu1);
        newItem.Caption := item;
        N10.Insert(0, newItem);
      end;
    end);
  fSettings.Load;

end;

procedure TOPPGuideForm.SetFilename(const Value: String);
begin
  fFilename := Value;

  actionGuideExport.Enabled := false;
  if length(self.Filename) = 0 then
  begin
    dxStatusBar1.Panels[0].text := 'New document';
  end else begin

    actionGuideExport.Enabled := FileExists(self.Filename);
    if FileExists(self.Filename) then
    begin
      dxStatusBar1.Panels[0].text := Format('Loaded file: %s', [self.Filename]);
    end else begin
      dxStatusBar1.Panels[0].text := 'Empty';
    end;
  end;

  DoUpdateStatusBarWidths();
end;

{ TOPPTreeDatasetHelpler }

procedure TOPPTreeDatasetHelpler.swapValues(AFieldName, AIDFieldName: String; sourceId, destinationId: Variant);
var
  fSourceDS, fDestinationDS: TClientDataSet;
  fSourceValue, fDestinatonValue: Variant;
begin
  if length(AFieldName) = 0 then
    exit;
  if length(AIDFieldName) = 0 then
    exit;
  if VarIsNull(sourceId) or VarIsEmpty(sourceId) then
    exit;
  if VarIsNull(destinationId) or VarIsEmpty(destinationId) then
    exit;

  fSourceDS := TClientDataSet.Create(nil);
  try
    fSourceDS.CloneCursor(self, false);
    fDestinationDS := TClientDataSet.Create(nil);
    try
      fDestinationDS.CloneCursor(self, false);

      fSourceDS.Filter := Format(' %s like ''%s''', [AIDFieldName, sourceId]);
      fSourceDS.Filtered := true;
      fSourceValue := fSourceDS.FieldByName(AFieldName).Value;

      fDestinationDS.Filter := Format(' %s like ''%s''', [AIDFieldName, destinationId]);
      fDestinationDS.Filtered := true;
      fDestinatonValue := fDestinationDS.FieldByName(AFieldName).Value;

      try
        fSourceDS.Edit;
        fSourceDS.FieldByName(AFieldName).Value := fDestinatonValue;
        fSourceDS.Post;
      except
      end;

      try
        fDestinationDS.Edit;
        fDestinationDS.FieldByName(AFieldName).Value := fSourceValue;
        fDestinationDS.Post;
      except
      end;
    finally
      fDestinationDS.Free;
    end;
  finally
    fSourceDS.Free;
  end;

end;

{ TForm1Helper }

function TForm1Helper.ScriptPanelIsVisible: Boolean;
var
  Value: Variant;
  intValue: Integer;
begin
  result := false;
  Value := DataSourceTreeView.DataSet.FieldByName('nodeType').Value;
  if VarIsNull(Value) or VarIsEmpty(Value) then
    exit;
  intValue := VarAsType(Value, vtInt64);
  result := (intValue = 1);
end;

{ TcxTreeListNodeHelper }

procedure TcxTreeListNodeHelper.DeleteAllDescendants;
var
  child: TcxTreeListNode;
begin
  child := self.getFirstChild;
  while assigned(child) do
  begin
    child.DeleteAllDescendants;
    child := self.GetNextChild(child);
  end;
end;

end.
