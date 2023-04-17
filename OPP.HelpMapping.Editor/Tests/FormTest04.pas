unit FormTest04;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridLevel, cxClasses, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, Datasnap.DBClient, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormTest4 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    Panel1: TPanel;
    Button1: TButton;
    cxGrid1DBTableView1id: TcxGridDBColumn;
    cxGrid1DBTableView1typename: TcxGridDBColumn;
    cxGrid1DBTableView1attributename: TcxGridDBColumn;
    cxGrid1DBTableView1linkname: TcxGridDBColumn;
    cxGrid1DBTableView1childtype: TcxGridDBColumn;
    cxGrid1DBTableView1roundvalue: TcxGridDBColumn;
    cxGrid1DBTableView1displaytextingrid: TcxGridDBColumn;
    cxGrid1DBTableView1displaytextinattrs: TcxGridDBColumn;
    cxGrid1DBTableView1defaultvalue: TcxGridDBColumn;
    cxGrid1DBTableView1regexp: TcxGridDBColumn;
    cxGrid1DBTableView1font: TcxGridDBColumn;
    cxGrid1DBTableView1defaultvalueunitid: TcxGridDBColumn;
    cxGrid1DBTableView1isbaseinconfig: TcxGridDBColumn;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClientDataSet1AfterOpen(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest4: TFormTest4;

implementation

uses OPP.Help.System.Codable.FormSizeSettings;

{$R *.dfm}

procedure TFormTest4.FormCreate(Sender: TObject);
begin
  self.readFormState;
end;

procedure TFormTest4.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormTest4.ClientDataSet1AfterOpen(DataSet: TDataSet);
begin
  cxGrid1DBTableView1.ApplyBestFit();
end;

procedure TFormTest4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.saveFormState;
end;

end.
