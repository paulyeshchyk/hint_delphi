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
    procedure actionAddChildRecordExecute(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actionExportExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
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
  pid: variant;
  id: String;
  fGUID: TGuid;
begin
  pid := DataSource1.DataSet.FieldByName('identifier').Value;
  DataSource1.DataSet.Insert;
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);
  DataSource1.DataSet.FieldByName('identifier').value := id;
  DataSource1.DataSet.FieldByName('pidentifier').Value := pid;
  DataSource1.DataSet.Post;
end;

procedure TForm1.actionAddRecordExecute(Sender: TObject);
var
  pid: variant;
  id: String;
  fGUID: TGuid;
begin
  pid := DataSource1.DataSet.FieldByName('pidentifier').Value;
  DataSource1.DataSet.Insert;
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);
  DataSource1.DataSet.FieldByName('identifier').value := id;
  DataSource1.DataSet.FieldByName('pidentifier').Value := pid;
  DataSource1.DataSet.Post;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  ClientDataSet1.LoadFromFile('C:\Users\pavel\Documents\1.xml');
end;

procedure TForm1.actionExportExecute(Sender: TObject);
begin
  if SaveDialog1.Execute(self.handle) then
  begin
//    ClientDataSet1.Post;
    ClientDataSet1.SaveToFile(SaveDialog1.FileName, dfXMLUTF8);
  end;
end;

procedure TForm1.actionReloadExecute(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile('C:\Users\pavel\Documents\1.xml');
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  actionAddChildRecord.Enabled := (datasource1.DataSet.RecordCount <> 0) and (not DataSource1.DataSet.FieldByName('identifier').IsNull);
end;

end.
