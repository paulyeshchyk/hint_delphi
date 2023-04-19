unit OPP.Buffer.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, Datasnap.DBClient, System.Actions, Vcl.ActnList, cxGridLevel, cxClasses, cxGridCustomView, cxGrid,
  Vcl.Menus;

type
  TOPPBufferForm = class(TForm)
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    ActionList1: TActionList;
    actionClose: TAction;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    ClientDataSet1order: TIntegerField;
    ClientDataSet1value: TStringField;
    ClientDataSet1isFixed: TBooleanField;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    actionClose1: TMenuItem;
    actionExportSettings: TAction;
    actionImportSettings: TAction;
    actionExportBuffer: TAction;
    actionImportBuffer: TAction;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N10: TMenuItem;
    actionNewRecord: TAction;
    actionDeleteRecord: TAction;
    actionWipeRecords: TAction;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    actionShowSettings: TAction;
    N15: TMenuItem;
    actionLoadRecords: TAction;
    actionSaveRecords: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionLoadRecordsExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionSaveRecordsExecute(Sender: TObject);
    procedure ClientDataSet1CalcFields(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OPPBufferForm: TOPPBufferForm;

implementation

uses
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Help.System.Clipboard;

{$R *.dfm}

procedure TOPPBufferForm.FormCreate(Sender: TObject);
begin
  self.readFormState;
end;

procedure TOPPBufferForm.actionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferForm.actionLoadRecordsExecute(Sender: TObject);
begin
  //
end;

procedure TOPPBufferForm.actionNewRecordExecute(Sender: TObject);
begin
  ClientDataSet1.Insert;
  ClientDataSet1.FieldByName('isFixed').AsBoolean := false;
  ClientDataSet1.Post;
end;

procedure TOPPBufferForm.actionSaveRecordsExecute(Sender: TObject);
begin
  //
end;

procedure TOPPBufferForm.ClientDataSet1CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('order').AsInteger := DataSet.RecNo;
end;

procedure TOPPBufferForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.saveFormState;
end;

end.
