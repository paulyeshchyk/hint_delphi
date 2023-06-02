unit OPP.Guide.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxPC, dxDockControl, cxControls, dxDockPanel, cxClasses, Vcl.ComCtrls, dxtree,
  Data.DB, Datasnap.DBClient, dxdbtree, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL,
  cxMaskEdit, cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog, cxInplaceContainer, cxDBTL,
  cxTLData, cxEdit, cxVGrid, cxDBVGrid, dxBar, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList;

type
  TForm1 = class(TForm)
    dxDockingManager1: TdxDockingManager;
    dxDockSite1: TdxDockSite;
    dxDockPanel2: TdxDockPanel;
    dxDockPanel1: TdxDockPanel;
    dxLayoutDockSite3: TdxLayoutDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Caption: TStringField;
    ClientDataSet1ActionText: TStringField;
    ClientDataSet1NodeType: TIntegerField;
    ClientDataSet1ReactionText: TStringField;
    ClientDataSet1ActualResultText: TStringField;
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
    cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow;
    cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn;
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionExportExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure cxDBTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State:
        TDragState; var Accept: Boolean);
    procedure cxDBTreeList1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
    function Count(parentID: Variant): Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  midaslib;

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
  ClientDataSet1.LoadFromFile('C:\Users\pavel\Application Data\Ascon\Gulfstream\Guide\opp.guide.xml');
end;

procedure TForm1.actionExportExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
    ClientDataSet1.SaveToFile(SaveDialog1.FileName, dfXMLUTF8);
  end;
end;

procedure TForm1.actionReloadExecute(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    ClientDataSet1.LoadFromFile(FileOpenDialog1.FileName);
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

procedure TForm1.cxDBTreeList1DragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
begin
//
end;

procedure TForm1.cxDBTreeList1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
//
end;

procedure TForm1.cxDBTreeList1InitInsertingRecord(Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
var
  fGUID: TGuid;
begin
  CreateGUID(fGUID);
  AFocusedNode.KeyValue := GUIDToString(fGUID);
  AHandled := true;
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  actionAddChildRecord.Enabled := (DataSource1.DataSet.RecordCount <> 0) and (not DataSource1.DataSet.FieldByName('identifier').IsNull);
end;

end.
