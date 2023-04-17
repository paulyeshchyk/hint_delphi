unit FormSchemeEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, Datasnap.DBClient, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, dxBar, cxDBExtLookupComboBox, cxDBLookupComboBox, Datasnap.Provider, cxButtonEdit;

type
  TOPPHintAttributeSchemeEditorForm = class(TForm)
    dxBarDockControl1: TdxBarDockControl;
    Panel1: TPanel;
    Button1: TButton;
    ActionList1: TActionList;
    ActionClose: TAction;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    SchemeDataSet: TClientDataSet;
    SchemeDataSource: TDataSource;
    SchemeDataSetClassName: TStringField;
    SchemeDataSetPropertyName: TStringField;
    actionAddRecord: TAction;
    actionSaveAs: TAction;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButtonSave: TdxBarButton;
    dxBarButtonNewRecord: TdxBarButton;
    actionReloadDataset: TAction;
    actionReloadRTTIDataset: TAction;
    actionSave: TAction;
    cxGrid1DBTableView1ClassName1: TcxGridDBColumn;
    cxGrid1DBTableView1PropertyName1: TcxGridDBColumn;
    procedure FormCreate(Sender: TObject);
    procedure actionAddRecordExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure actionReloadDatasetExecute(Sender: TObject);
    procedure actionSaveAsExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure cxGrid1DBTableView1ClassNamePropertiesChange(Sender: TObject);
    procedure cxGrid1DBTableView1PropertyNamePropertiesInitPopup(Sender: TObject);
  private
    function GetSchemeXMLFilePath: String;
    function GetRTTIXMLFilePath: String;
    { Private declarations }
    property SchemeXMLFilePath: String read GetSchemeXMLFilePath;
    property RTTIXMLFilePath: String read GetRTTIXMLFilePath;
  public
    { Public declarations }
  end;

var
  OPPHintAttributeSchemeEditorForm: TOPPHintAttributeSchemeEditorForm;

implementation

uses System.IOUtils, OPP.Help.System.Files;

const
  SRTTIFileName = '.\Документация\RTTI.xml';
  SSchemeFileName = '.\Документация\hint_attrs_scheme.xml';

{$R *.dfm}

procedure TOPPHintAttributeSchemeEditorForm.FormCreate(Sender: TObject);
begin
  actionReloadRTTIDataset.Execute;
  actionReloadDataset.Execute;
end;

function TOPPHintAttributeSchemeEditorForm.GetRTTIXMLFilePath: String;
begin
  result := TOPPHelpSystemFilesHelper.AbsolutePath(SRTTIFileName);
end;

function TOPPHintAttributeSchemeEditorForm.GetSchemeXMLFilePath: String;
begin
  result := TOPPHelpSystemFilesHelper.AbsolutePath(SSchemeFileName)
end;

procedure TOPPHintAttributeSchemeEditorForm.actionAddRecordExecute(Sender: TObject);
begin
  SchemeDataSet.Insert;
  SchemeDataSet.Post;
end;

procedure TOPPHintAttributeSchemeEditorForm.ActionCloseExecute(Sender: TObject);
begin
  actionSave.Execute;
  close;
end;

procedure TOPPHintAttributeSchemeEditorForm.actionReloadDatasetExecute(Sender: TObject);
var
  fField: TField;
  col: TcxGridDBColumn;
begin
  SchemeDataSet.Active := false;
  SchemeDataSet.LoadFromFile(self.SchemeXMLFilePath);
  (cxGrid1DBTableView1.DataController as IcxCustomGridDataController).CreateAllItems(true);
  SchemeDataSet.Active := true;
  cxGrid1DBTableView1.ApplyBestFit();
end;

procedure TOPPHintAttributeSchemeEditorForm.actionSaveAsExecute(Sender: TObject);
var
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(Self);
  try
    saveDialog.InitialDir := TPath.GetDirectoryName(self.SchemeXMLFilePath);
    if saveDialog.Execute(Handle) then
    begin
      SchemeDataSet.SaveToFile(saveDialog.FileName, dfXMLUTF8);
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TOPPHintAttributeSchemeEditorForm.actionSaveExecute(Sender: TObject);
begin
  SchemeDataSet.SaveToFile(self.SchemeXMLFilePath, dfXMLUTF8);
end;

procedure TOPPHintAttributeSchemeEditorForm.cxGrid1DBTableView1ClassNamePropertiesChange(Sender: TObject);
begin
  // RTTIPropertyDataSet.Refresh;
end;

procedure TOPPHintAttributeSchemeEditorForm.cxGrid1DBTableView1PropertyNamePropertiesInitPopup(Sender: TObject);
//var
//  fFilter: String;
//  row: Integer;
//  s: String;
begin
//  row := cxGrid1DBTableView1.DataController.FocusedDataRowIndex;
//  s := cxGrid1DBTableView1.DataController.GetDisplayText(row, 0);
//  fFilter := Format('UPPER(typename) LIKE ''%%%s%%''', [UPPERCASE(s)]);
//  RTTIPropertyDataSet.Filter := fFilter;
//  RTTIPropertyDataSet.Filtered := true;
end;

end.
