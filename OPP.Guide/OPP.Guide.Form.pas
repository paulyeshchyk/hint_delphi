unit OPP.Guide.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxPC, dxDockControl, cxControls, dxDockPanel, cxClasses, Vcl.ComCtrls, dxtree,
  Data.DB, Datasnap.DBClient, dxdbtree, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxMaskEdit, cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, cxInplaceContainer, cxDBTL,
  cxTLData, cxEdit, cxVGrid, cxDBVGrid, dxBar, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, cxContainer,
  cxTextEdit, cxMemo, dxScrollbarAnnotations, cxDBLookupComboBox;

type
  TForm1 = class(TForm)
    dxDockingManager1: TdxDockingManager;
    dxDockSite1: TdxDockSite;
    dxDockPanel2: TdxDockPanel;
    dxDockPanel1: TdxDockPanel;
    dxLayoutDockSite3: TdxLayoutDockSite;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1ActionText: TStringField;
    ClientDataSet1NodeType: TIntegerField;
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
    ClientDataSet1Identifier: TStringField;
    ClientDataSet1PIdentifier: TStringField;
    actionReload: TAction;
    dxBarButton4: TdxBarButton;
    FileOpenDialog1: TFileOpenDialog;
    actionOpen: TAction;
    ClientDataSet1Order: TIntegerField;
    cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn;
    dxDockPanel3: TdxDockPanel;
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
    dxDockPanel7: TdxDockPanel;
    cxMemo1: TcxMemo;
    dxLayoutDockSite2: TdxLayoutDockSite;
    ClientDataSet1Caption: TWideStringField;
    DataSource2: TDataSource;
    ClientDataSet2: TClientDataSet;
    ClientDataSet2id: TIntegerField;
    ClientDataSet2caption: TWideStringField;
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionExportExecute(Sender: TObject);
    procedure actionNewExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionRunSelectedExecute(Sender: TObject);
    procedure cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure cxDBTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxDBTreeList1KeyPress(Sender: TObject; var Key: Char);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
    function Count(parentID: Variant): Integer;
  public
    { Public declarations }
  end;

  TOPPTreeDatasetHelpler = class helper for TClientDataSet
    procedure swapValues(AFieldName: String; AIDFieldName: String; sourceId, destinationId: Variant);
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
  id: String;
  fGUID: TGuid;
  cnt: Integer;
begin
  pid := DataSource1.DataSet.FieldByName('identifier').Value;
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);

  cnt := Count(pid);

  DataSource1.DataSet.Insert;
  DataSource1.DataSet.FieldByName('identifier').Value := id;
  DataSource1.DataSet.FieldByName('pidentifier').Value := pid;
  DataSource1.DataSet.FieldByName('Caption').Value := id;
  DataSource1.DataSet.FieldByName('Order').Value := cnt;
  DataSource1.DataSet.Post;
end;

procedure TForm1.actionAddRecordExecute(Sender: TObject);
var
  pid: Variant;
  id: String;
  fGUID: TGuid;
  cnt: Integer;
begin
  pid := DataSource1.DataSet.FieldByName('pidentifier').Value;
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);

  cnt := Count(pid);

  DataSource1.DataSet.Insert;
  DataSource1.DataSet.FieldByName('identifier').Value := id;
  DataSource1.DataSet.FieldByName('pidentifier').Value := pid;
  DataSource1.DataSet.FieldByName('Caption').Value := id;
  DataSource1.DataSet.FieldByName('Order').Value := cnt;
  DataSource1.DataSet.Post;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile('C:\Users\paul\Application Data\Ascon\Gulfstream\Guide\opp.guide.xml');
end;

procedure TForm1.actionExportExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
    ClientDataSet1.SaveToFile(SaveDialog1.FileName, dfXMLUTF8);
  end;
end;

procedure TForm1.actionNewExecute(Sender: TObject);
begin
  ClientDataSet1.DisableControls;
  try
    ClientDataSet1.EmptyDataSet;
  finally
    ClientDataSet1.EnableControls;
  end;
end;

procedure TForm1.actionReloadExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    ClientDataSet1.LoadFromFile(FileOpenDialog1.FileName);
end;

procedure TForm1.actionRunSelectedExecute(Sender: TObject);
var
  ident: Variant;
begin
  ident := ClientDataSet1.FieldByName('identifier').Value;
  cxMemo1.Clear;
  TOPPGuideExecutor.run(ClientDataSet1, ident,
    procedure(AText: String)
    begin
      cxMemo1.Lines.Add(AText);
    end);
end;

function TForm1.Count(parentID: Variant): Integer;
var
  cloned: TClientDataSet;
  fFilter: String;
begin
  result := 0;
  if (VarIsNull(parentID) or VarIsEmpty(parentID)) then
  begin
    fFilter := Format('pidentifier IS NULL', []);
  end else begin
    fFilter := Format('pidentifier LIKE ''%s''', [parentID]);
  end;
  cloned := TClientDataSet.Create(nil);
  try
    cloned.CloneCursor(ClientDataSet1, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    result := cloned.RecordCount;
  finally
    cloned.Free;
  end;

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
    ClientDataSet1.swapValues('Order', 'identifier', kv1, kv2);
    Key := 0;//VK_ESCAPE;
  end;

  //
end;

procedure TForm1.cxDBTreeList1KeyPress(Sender: TObject; var Key: Char);
begin
  //
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  actionAddChildRecord.Enabled := (DataSource1.DataSet.RecordCount <> 0) and (not DataSource1.DataSet.FieldByName('identifier').IsNull);
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

end.
