unit OPP.Guide.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxPC, dxDockControl, cxControls, dxDockPanel, cxClasses, Vcl.ComCtrls, dxtree,
  Data.DB, Datasnap.DBClient, dxdbtree, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxMaskEdit, cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, cxInplaceContainer, cxDBTL,
  cxTLData, cxEdit, cxVGrid, cxDBVGrid, dxBar, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, cxContainer,
  cxTextEdit, cxMemo, dxScrollbarAnnotations, cxDBLookupComboBox, cxBlobEdit, cxDropDownEdit, cxDBEdit;

type
  TForm1 = class(TForm)
    dxDockingManager1: TdxDockingManager;
    dxDockSite1: TdxDockSite;
    dxDockPanelProperties: TdxDockPanel;
    dxDockPanelTreeView: TdxDockPanel;
    dxLayoutDockSite3: TdxLayoutDockSite;
    DataSourceTreeView: TDataSource;
    DataSetTreeView: TClientDataSet;
    DataSetTreeViewActionText: TStringField;
    DataSetTreeViewNodeType: TIntegerField;
    cxDBTreeList1: TcxDBTreeList;
    cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow2: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow3: TcxDBEditorRow;
    cxDBVerticalGrid1DBEditorRow4: TcxDBEditorRow;
    dxBarDockControl1: TdxBarDockControl;
    dxBarManager1: TdxBarManager;
    ActionList1: TActionList;
    actionAddRecord: TAction;
    actionAddChildRecord: TAction;
    actionRemoveRecord: TAction;
    actionExport: TAction;
    SaveDialog1: TSaveDialog;
    dxBarManager1Bar1: TdxBar;
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
    cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn;
    dxDockPanelScript: TdxDockPanel;
    dxDockPanel4: TdxDockPanel;
    dxDockPanel5: TdxDockPanel;
    dxDockPanel6: TdxDockPanel;
    cxDBVerticalGrid1DBEditorRow6: TcxDBEditorRow;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
    dxTabContainerDockSite1: TdxTabContainerDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    dxBarButton5: TdxBarButton;
    actionNew: TAction;
    actionRunSelected: TAction;
    actionRunAll: TAction;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxDockPanelOutputLog: TdxDockPanel;
    cxMemo1: TcxMemo;
    dxLayoutDockSite2: TdxLayoutDockSite;
    DataSetTreeViewCaption: TWideStringField;
    DataSourceNodeType: TDataSource;
    DataSetNodeType: TClientDataSet;
    DataSetNodeTypeid: TIntegerField;
    DataSetNodeTypecaption: TWideStringField;
    DataSetTreeViewScript: TBlobField;
    cxDBMemo1: TcxDBMemo;
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionExportExecute(Sender: TObject);
    procedure actionNewExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionRunAllExecute(Sender: TObject);
    procedure actionRunSelectedExecute(Sender: TObject);
    procedure cxDBTreeList1Change(Sender: TObject);
    procedure cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DataSourceTreeViewDataChange(Sender: TObject; Field: TField);
    procedure cxDBMemo1PropertiesChange(Sender: TObject);
    procedure DataSetTreeViewAfterOpen(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TOPPTreeDatasetHelpler = class helper for TClientDataSet
    procedure swapValues(AFieldName: String; AIDFieldName: String; sourceId, destinationId: Variant);
  end;

  TForm1Helper = class helper for TForm1
    procedure AddChild(AParentIdentifier: Variant);
    function subsCount(AIdentifier: Variant): Integer;
    function selectedNodeSubsCount: Integer;
    function selectedNodeIsRunnable: Boolean;
  end;

var
  Form1: TForm1;

implementation

uses
  OPP.Help.System.Str,
  midaslib, OPP.Guide.executor;

{$R *.dfm}

procedure TForm1.actionAddChildRecordExecute(Sender: TObject);
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('identifier').Value;
  AddChild(pid);
end;

procedure TForm1.actionAddRecordExecute(Sender: TObject);
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('pidentifier').Value;
  AddChild(pid);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxDBTreeList1.BeginUpdate;
  DataSetTreeView.LoadFromFile('C:\Users\paul\Application Data\Ascon\Gulfstream\Guide\opp.guide.xml');
  cxDBTreeList1.EndUpdate;
end;

procedure TForm1.actionExportExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
    DataSetTreeView.SaveToFile(SaveDialog1.FileName, dfXMLUTF8);
  end;
end;

procedure TForm1.actionNewExecute(Sender: TObject);
begin
  DataSetTreeView.DisableControls;
  try
    DataSetTreeView.EmptyDataSet;
  finally
    DataSetTreeView.EnableControls;
  end;
end;

procedure TForm1.actionReloadExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    DataSetTreeView.LoadFromFile(FileOpenDialog1.FileName);
end;

procedure TForm1.actionRunAllExecute(Sender: TObject);
var
  ident: Variant;
begin
  ident := DataSetTreeView.FieldByName('identifier').Value;
  cxMemo1.Clear;
  TOPPGuideExecutor.run(DataSetTreeView, ident, true,
    procedure(AText: String)
    begin
      cxMemo1.Lines.Add(AText);
    end);
end;

procedure TForm1.actionRunSelectedExecute(Sender: TObject);
var
  ident: Variant;
begin
  ident := DataSetTreeView.FieldByName('identifier').Value;
  cxMemo1.Clear;
  TOPPGuideExecutor.run(DataSetTreeView, ident, false,
    procedure(AText: String)
    begin
      cxMemo1.Lines.Add(AText);
    end);
end;

procedure TForm1.cxDBMemo1PropertiesChange(Sender: TObject);
begin
  if DataSetTreeView.Modified then
    DataSetTreeView.Post;
end;

procedure TForm1.cxDBTreeList1Change(Sender: TObject);
begin
  actionRunAll.Enabled := (selectedNodeSubsCount > 0);
  dxDockPanelScript.Visible := selectedNodeIsRunnable();
end;

procedure TForm1.cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //
end;

procedure TForm1.cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  OutputDebugString(Format('%d', [Integer(State)]).toWideChar);
end;

procedure TForm1.cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
var
  fGUID: TGuid;
begin
  CreateGUID(fGUID);
  AFocusedNode.KeyValue := GUIDToString(fGUID);
  AHandled := true;
end;

procedure TForm1.cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TForm1.DataSetTreeViewAfterOpen(DataSet: TDataSet);
begin
  cxDBTreeList1cxDBTreeListColumn1.Width := cxDBTreeList1.Width;
end;

procedure TForm1.DataSourceTreeViewDataChange(Sender: TObject; Field: TField);
begin
  actionAddChildRecord.Enabled := (DataSourceTreeView.DataSet.RecordCount <> 0) and (not DataSourceTreeView.DataSet.FieldByName('identifier').IsNull);
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

  cnt := subsCount(AParentIdentifier);

  DataSourceTreeView.DataSet.Insert;
  DataSourceTreeView.DataSet.FieldByName('identifier').Value := id;
  DataSourceTreeView.DataSet.FieldByName('pidentifier').Value := AParentIdentifier;
  DataSourceTreeView.DataSet.FieldByName('Caption').Value := id;
  DataSourceTreeView.DataSet.FieldByName('Order').Value := cnt;
  DataSourceTreeView.DataSet.Post;
end;

function TForm1Helper.selectedNodeIsRunnable: Boolean;
var
  value: Variant;
  intValue: Integer;
begin
  result := false;
  value := DataSourceTreeView.DataSet.FieldByName('nodeType').Value;
  if VarIsNull(value) or VarIsEmpty(Value) then
    exit;
  intValue := VarAsType(value, vtInt64);
  result := (intValue = 1);
end;

function TForm1Helper.selectedNodeSubsCount: Integer;
var
  pid: Variant;
begin
  pid := DataSourceTreeView.DataSet.FieldByName('identifier').Value;
  result := subsCount(pid);
end;

function TForm1Helper.subsCount(AIdentifier: Variant): Integer;
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

end.
