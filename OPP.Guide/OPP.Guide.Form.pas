﻿unit OPP.Guide.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxPC, dxDockControl, cxControls, dxDockPanel, cxClasses, Vcl.ComCtrls, dxtree,
  Data.DB, Datasnap.DBClient, dxdbtree, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxMaskEdit, cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, cxInplaceContainer, cxDBTL,
  cxTLData, cxEdit, cxVGrid, cxDBVGrid, dxBar, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, cxContainer,
  cxTextEdit, cxMemo, cxDBLookupComboBox, cxBlobEdit, cxDropDownEdit, cxDBEdit, dxStatusBar,

  OPP.Help.Vcl.PanelTrigger,
  OPP.Guide.Settings,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Guide.Scripter, ScrMemo, ScrMps, Vcl.Menus;

type
  TOPPGuideForm = class(TForm)
    dxDockingManager1: TdxDockingManager;
    DataSourceTreeView: TDataSource;
    DataSetTreeView: TClientDataSet;
    DataSetTreeViewActionText: TStringField;
    DataSetTreeViewNodeType: TIntegerField;
    dxBarManager1: TdxBarManager;
    ActionList1: TActionList;
    actionAddRecord: TAction;
    actionAddChildRecord: TAction;
    actionRemoveRecord: TAction;
    actionExport: TAction;
    SaveDialog1: TSaveDialog;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    ImageList1: TImageList;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    DataSetTreeViewIdentifier: TStringField;
    DataSetTreeViewPIdentifier: TStringField;
    actionReload: TAction;
    dxBarButton4: TdxBarButton;
    FileOpenDialog1: TFileOpenDialog;
    actionOpen: TAction;
    DataSetTreeViewOrder: TIntegerField;
    dxBarButton5: TdxBarButton;
    actionNew: TAction;
    actionRunSelected: TAction;
    actionRunAll: TAction;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    DataSetTreeViewCaption: TWideStringField;
    DataSourceNodeType: TDataSource;
    DataSetNodeType: TClientDataSet;
    DataSetNodeTypeid: TIntegerField;
    DataSetNodeTypecaption: TWideStringField;
    DataSetTreeViewScript: TBlobField;
    dxBarManager1Bar2: TdxBar;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    actionSaveScript: TAction;
    actionRunScript: TAction;
    dxBarButton10: TdxBarButton;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxLayoutDockSite3: TdxLayoutDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    dxDockPanel5: TdxDockPanel;
    dxDockPanel6: TdxDockPanel;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
    dxDockPanelTreeView: TdxDockPanel;
    cxDBTreeList1: TcxDBTreeList;
    cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn;
    cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn;
    dxDockPanelScript: TdxDockPanel;
    dxBarDockControl2: TdxBarDockControl;
    dxDockPanelProperties: TdxDockPanel;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    cxDBVerticalGrid1DBEditorRow6: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow3: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow4: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow2: TcxDBEditorRow;
    dxDockPanelOutputLog: TdxDockPanel;
    cxMemo1: TcxMemo;
    dxStatusBar1: TdxStatusBar;
    cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow;
    actionShowFindPanel: TAction;
    ScrPascalMemoStyler1: TScrPascalMemoStyler;
    ScrMemoSource1: TScrMemoSource;
    ScrMemo1: TScrMemo;
    dxBarDockControl3: TdxBarDockControl;
    dxBarButton11: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    actionAddRecord1: TMenuItem;
    actionAddChildRecord1: TMenuItem;
    actionNew1: TMenuItem;
    N6: TMenuItem;
    actionOpen1: TMenuItem;
    actionRemoveRecord1: TMenuItem;
    N7: TMenuItem;
    actionRunAll1: TMenuItem;
    actionRunSelected1: TMenuItem;
    actionRunScript1: TMenuItem;
    actionClose: TAction;
    actionClose1: TMenuItem;
    actionHelp: TAction;
    actionHelp1: TMenuItem;
    N8: TMenuItem;
    actionExport1: TMenuItem;
    N11: TMenuItem;
    actionCompileScript: TAction;
    actionCompileScript1: TMenuItem;
    dxStatusBar2: TdxStatusBar;
    dxBarButton12: TdxBarButton;
    N9: TMenuItem;
    N10: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    actionClearRecentList: TAction;
    PopupMenu1: TPopupMenu;
    actionAddRecord2: TMenuItem;
    actionAddChildRecord2: TMenuItem;
    actionRemoveRecord2: TMenuItem;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure actionClearRecentListExecute(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionCompileScriptExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionExportExecute(Sender: TObject);
    procedure actionHelpExecute(Sender: TObject);
    procedure actionNewExecute(Sender: TObject);
    procedure actionOpenExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionRemoveRecordExecute(Sender: TObject);
    procedure actionRunAllExecute(Sender: TObject);
    procedure actionRunScriptExecute(Sender: TObject);
    procedure actionRunSelectedExecute(Sender: TObject);
    procedure actionSaveScriptExecute(Sender: TObject);
    procedure actionShowFindPanelExecute(Sender: TObject);
    procedure cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxDBTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure DataSetTreeViewAfterOpen(DataSet: TDataSet);
    procedure DataSetTreeViewAfterPost(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataSetTreeViewBeforeEdit(DataSet: TDataSet);
    procedure cxDBVerticalGrid1DBEditorRow6EditPropertiesEditValueChanged(Sender: TObject);
    procedure DataSetTreeViewAfterApplyUpdates(Sender: TObject; var OwnerData: OLEVariant);
    procedure DataSetTreeViewAfterScroll(DataSet: TDataSet);
    procedure dxStatusBar1Resize(Sender: TObject);
    procedure ScrMemo1Change(Sender: TObject);
    procedure ScrMemo1CursorChange(Sender: TObject);
  private
    { Private declarations }
    fSettings: IOPPGuideSettings;
    fScripter: IOPPGuideScripter;
    fSaveActionIsInProgress: Boolean;
    fScriptPanelTrigger: TOPPHelpVCLPanelTrigger;
    fPanelTriggerContainer: TOPPHelpVCLPanelTriggerContainer;
    procedure BuildPanelTriggers(AMenuItem: TMenuItem);
    procedure ReadDefaults;
    procedure DoUpdateStatusBarWidths;
    procedure LoadDatasetContent(AFilename: String);
    class function GetApplicationTitle: String; static;
  public
    { Public declarations }
    property SaveActionIsInProgress: Boolean read fSaveActionIsInProgress write fSaveActionIsInProgress default false;
    class property ApplicationTitle: String read GetApplicationTitle;
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

  midaslib, OPP.Guide.executor;

type
  TOPPTreeDatasetHelpler = class helper for TClientDataSet
    procedure swapValues(AFieldName: String; AIDFieldName: String; sourceId, destinationId: Variant);
  end;

  TForm1Helper = class helper for TOPPGuideForm
    procedure AddChild(AParentIdentifier: Variant);
    function SubsCount(AIdentifier: Variant): Integer;
    function SelectedNodeSubsCount: Integer;
    function ScriptPanelIsVisible: Boolean;
  end;

  TcxTreeListNodeHelper = class helper for TcxTreeListNode
    procedure DeleteAllDescendants;
  end;

{$R *.dfm}

procedure TOPPGuideForm.actionAddChildRecordExecute(Sender: TObject);
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('identifier').Value;
  AddChild(pid);
end;

procedure TOPPGuideForm.actionAddRecordExecute(Sender: TObject);
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('pidentifier').Value;
  AddChild(pid);
end;

procedure TOPPGuideForm.actionClearRecentListExecute(Sender: TObject);
begin
  fSettings.ClearHierarchyFilenameRecentList;
end;

procedure TOPPGuideForm.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPGuideForm.actionCompileScriptExecute(Sender: TObject);
var
  ident: Variant;
begin
  actionSaveScript.Execute;

  ident := DataSetTreeView.FieldByName('identifier').Value;
  TOPPGuideExecutor.compile(DataSetTreeView, ident, false, fScripter,
    procedure(AText: String)
    begin
      cxMemo1.Lines.Add(AText);
    end);
end;

procedure TOPPGuideForm.FormCreate(Sender: TObject);
begin
  actionHelp.Caption := Application.BuildNumber;
  self.Caption := TOPPGuideForm.GetApplicationTitle;

  fScripter := TOPPGuideScripterDC.Create;

  fSettings := TOPPGuideSettings.Create;

  ReadDefaults;

  BuildPanelTriggers(N2);

  DoUpdateStatusBarWidths;

end;

class function TOPPGuideForm.GetApplicationTitle: String;
begin
  result := SDefaultCaption;
end;

procedure TOPPGuideForm.LoadDatasetContent(AFilename: String);
var
  fDatasetXMLFile: String;
begin
  if TOPPHelpSystemFilesHelper.FilenameHasPath(AFilename) then
    fDatasetXMLFile := AFilename
  else
    fDatasetXMLFile := TOPPHelpSystemFilesHelper.GetOPPGuidePath(AFilename);

  if FileExists(fDatasetXMLFile) then
  begin
    cxDBTreeList1.BeginUpdate;
    DataSetTreeView.LoadFromFile(fDatasetXMLFile);
    cxDBTreeList1.EndUpdate;
    dxStatusBar1.Panels[0].Text := Format('Loaded file: %s', [fDatasetXMLFile]);
  end else begin
    eventLogger.Error(Format('File not found: %s', [fDatasetXMLFile]), kContext);
    dxStatusBar1.Panels[0].Text := 'Empty';
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

procedure TOPPGuideForm.actionExportExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
    DataSetTreeView.SaveToFile(SaveDialog1.FileName, dfXMLUTF8);
    dxStatusBar1.Panels[0].Text := Format('Loaded file: %s', [SaveDialog1.FileName]);
  end;
end;

procedure TOPPGuideForm.actionHelpExecute(Sender: TObject);
begin
  Application.openMarketingWebPage;
end;

procedure TOPPGuideForm.actionNewExecute(Sender: TObject);
begin
  DataSetTreeView.DisableControls;
  try
    DataSetTreeView.EmptyDataSet;
    dxStatusBar1.Panels[0].Text := 'New document';
  finally
    DataSetTreeView.EnableControls;
  end;
end;

procedure TOPPGuideForm.actionOpenExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    fSettings.SetDefaultHierarchyFilename(FileOpenDialog1.FileName);
    LoadDatasetContent(FileOpenDialog1.FileName);
  end;
end;

procedure TOPPGuideForm.actionReloadExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    LoadDatasetContent(FileOpenDialog1.FileName);
  end;
end;

procedure TOPPGuideForm.actionRemoveRecordExecute(Sender: TObject);
begin
  cxDBTreeList1.BeginUpdate;
  cxDBTreeList1.FocusedNode.DeleteAllDescendants;
  cxDBTreeList1.FocusedNode.Delete;
  cxDBTreeList1.EndUpdate;
end;

procedure TOPPGuideForm.actionRunAllExecute(Sender: TObject);
var
  ident: Variant;
begin
  ident := DataSetTreeView.FieldByName('identifier').Value;
  cxMemo1.Clear;

  try
    TOPPGuideExecutor.run(DataSetTreeView, ident, true, fScripter,
      procedure(AText: String)
      begin
        cxMemo1.Lines.Add(AText);
      end);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPGuideForm.actionRunScriptExecute(Sender: TObject);
begin
  actionSaveScript.Execute;
  actionRunSelected.Execute;
end;

procedure TOPPGuideForm.actionRunSelectedExecute(Sender: TObject);
var
  ident: Variant;
  fScriptResult: Variant;
begin
  ident := DataSetTreeView.FieldByName('identifier').Value;
  cxMemo1.Clear;

  try
    TOPPGuideExecutor.run(DataSetTreeView, ident, false, fScripter,
      procedure(AText: String)
      begin
        cxMemo1.Lines.Add(AText);
      end);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPGuideForm.actionSaveScriptExecute(Sender: TObject);
begin
  self.SaveActionIsInProgress := true;
  try
    DataSourceTreeView.DataSet.DisableControls;
    if DataSourceTreeView.DataSet.State in [dsEdit, dsInsert, dsNewValue] then
      DataSourceTreeView.DataSet.Post;
    ScrMemoSource1.Modified := false;
    dxStatusBar1.Panels[2].Text := 'Insert';
    DoUpdateStatusBarWidths;
    DataSourceTreeView.DataSet.EnableControls;
  finally
    self.SaveActionIsInProgress := false;
  end;
end;

procedure TOPPGuideForm.actionShowFindPanelExecute(Sender: TObject);
begin
  cxDBTreeList1.SetFocus;
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

procedure TOPPGuideForm.cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //
end;

procedure TOPPGuideForm.cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  OutputDebugString(Format('%d', [Integer(State)]).toWideChar);
end;

procedure TOPPGuideForm.cxDBTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  actionRunAll.Enabled := (SelectedNodeSubsCount > 0);
  if Assigned(fScriptPanelTrigger) then
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
  if not Assigned(node) or not(node is TcxDBTreeListNode) then
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

  if Assigned(nodeToReplace) and (nodeToReplace is TcxDBTreeListNode) then
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
  actionSaveScript.Enabled := false;
end;

procedure TOPPGuideForm.DataSetTreeViewAfterScroll(DataSet: TDataSet);
var
  fStream: TStream;
begin
  actionAddChildRecord.Enabled := (DataSourceTreeView.DataSet.RecordCount <> 0) and (not DataSourceTreeView.DataSet.FieldByName('identifier').IsNull);
  actionRemoveRecord.Enabled := Assigned(cxDBTreeList1.FocusedNode);

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
  actionSaveScript.Enabled := true;
end;

procedure TOPPGuideForm.DoUpdateStatusBarWidths;
var
  l1, l2: Integer;
begin
  l1 := 40 + dxStatusBar1.Canvas.TextWidth(dxStatusBar1.Panels[0].Text);
  l2 := 40 + dxStatusBar1.Canvas.TextWidth(dxStatusBar1.Panels[2].Text);
  dxStatusBar1.Panels[0].Width := l1;
  dxStatusBar1.Panels[1].Width := dxStatusBar1.Width - l1 - l2;
  dxStatusBar1.Panels[2].Width := l2;

  dxStatusBar2.Panels[0].Width := 40 + dxStatusBar2.Canvas.TextWidth('9999:9999');
end;

procedure TOPPGuideForm.dxStatusBar1Resize(Sender: TObject);
begin
  DoUpdateStatusBarWidths;
end;

procedure TOPPGuideForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  fScripter := nil;
  fScriptPanelTrigger := nil;
  // fPanelTriggerContainer.Free;
  fSettings := nil;
end;

procedure TOPPGuideForm.ScrMemo1Change(Sender: TObject);
var
  fStream: TStream;
begin
  if self.SaveActionIsInProgress then
    exit;

  if not ScrMemoSource1.Modified then
    exit;

  dxStatusBar1.Panels[2].Text := 'Modified';
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
  dxStatusBar2.Panels[0].Text := Format('%4.d:%d', [ScrMemo1.CurY + 1, ScrMemo1.CurX + 1]);
end;

{ TOPPTreeDatasetHelpler }

procedure TOPPTreeDatasetHelpler.swapValues(AFieldName, AIDFieldName: String; sourceId, destinationId: Variant);
var
  fSourceDS, fDestinationDS: TClientDataSet;
  fSourceValue, fDestinatonValue: Variant;
begin
  if Length(AFieldName) = 0 then
    exit;
  if Length(AIDFieldName) = 0 then
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

procedure TForm1Helper.AddChild(AParentIdentifier: Variant);
var
  id: String;
  fGUID: TGuid;
  cnt: Integer;
begin
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);

  cnt := SubsCount(AParentIdentifier);

  DataSourceTreeView.DataSet.Insert;
  try
    try
      DataSourceTreeView.DataSet.FieldByName('identifier').Value := id;
      DataSourceTreeView.DataSet.FieldByName('pidentifier').Value := AParentIdentifier;
      DataSourceTreeView.DataSet.FieldByName('Caption').AsString := id;
      DataSourceTreeView.DataSet.FieldByName('Order').AsInteger := cnt;
      DataSourceTreeView.DataSet.FieldByName('NodeType').AsInteger := 0;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    DataSourceTreeView.DataSet.Post;
  end;
end;

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

function TForm1Helper.SelectedNodeSubsCount: Integer;
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('identifier').Value;
  result := SubsCount(pid);
end;

function TForm1Helper.SubsCount(AIdentifier: Variant): Integer;
var
  cloned: TClientDataSet;
  fFilter: String;
begin
  result := 0;

  if not DataSetTreeView.Active then
    exit;

  if (VarIsNull(AIdentifier) or VarIsEmpty(AIdentifier)) then
  begin
    fFilter := Format('pidentifier IS NULL', []);
  end else begin
    fFilter := Format('pidentifier LIKE ''%s''', [AIdentifier]);
  end;
  cloned := TClientDataSet.Create(nil);
  try
    cloned.CloneCursor(DataSetTreeView, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    result := cloned.RecordCount;
  finally
    cloned.Free;
  end;
end;

{ TcxTreeListNodeHelper }

procedure TcxTreeListNodeHelper.DeleteAllDescendants;
var
  child: TcxTreeListNode;
begin
  child := self.getFirstChild;
  while Assigned(child) do begin
    child.DeleteAllDescendants;
    child := self.GetNextChild(child);
  end;
end;

end.
