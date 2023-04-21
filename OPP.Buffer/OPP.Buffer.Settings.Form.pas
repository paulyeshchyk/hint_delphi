unit OPP.Buffer.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, cxButtons, Vcl.ExtCtrls, cxControls, cxContainer, cxEdit, cxCustomListBox, cxCheckListBox,
  cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, cxCheckBox, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, Vcl.ComCtrls, Datasnap.DBClient;

type
  TOPPBufferSettingsForm = class(TForm)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    cxButton1: TcxButton;
    ActionList1: TActionList;
    actionClose: TAction;
    cxCheckBox1: TcxCheckBox;
    cxSpinEdit1: TcxSpinEdit;
    cxLabel1: TcxLabel;
    GroupBox2: TGroupBox;
    cxCheckBox2: TcxCheckBox;
    actionUnlimitedRecordsCount: TAction;
    actionAddRecordsFromOtherApps: TAction;
    actionSaveSettings: TAction;
    cxCheckBox3: TcxCheckBox;
    GroupBox3: TGroupBox;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxLabel2: TcxLabel;
    HotKey1: THotKey;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet2: TClientDataSet;
    ClientDataSet2SortTypeID: TSmallintField;
    ClientDataSet2SortTypeName: TStringField;
    cxGrid1DBTableView1Column1: TcxGridDBColumn;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1DBTableView1Column3: TcxGridDBColumn;
    ClientDataSet3: TClientDataSet;
    ClientDataSet3FieldNameID: TSmallintField;
    ClientDataSet3FieldNameCaption: TStringField;
    ClientDataSet1SortTypeID: TSmallintField;
    ClientDataSet1SortTypeCaption: TStringField;
    ClientDataSet1SortOrder: TSmallintField;
    ClientDataSet1FieldNameID: TSmallintField;
    ClientDataSet1FieldNameCaption: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionSaveSettingsExecute(Sender: TObject);
    procedure cxCheckBox1PropertiesChange(Sender: TObject);
    procedure HotKey1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OPPBufferSettingsForm: TOPPBufferSettingsForm;

implementation

{$R *.dfm}

procedure TOPPBufferSettingsForm.FormCreate(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile('D:\sort_settings.xml');
  ClientDataset3.LoadFromFile('D:\columns.xml');
end;

procedure TOPPBufferSettingsForm.actionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPBufferSettingsForm.actionSaveSettingsExecute(Sender: TObject);
begin
//  ClientDataset3.SaveToFile('D:\columns.xml');
//  ClientDataset1.SaveToFile('D:\sort_settings.xml');

  Close;
end;

procedure TOPPBufferSettingsForm.cxCheckBox1PropertiesChange(Sender: TObject);
begin
  actionUnlimitedRecordsCount.Execute;
end;

procedure TOPPBufferSettingsForm.HotKey1Change(Sender: TObject);
var shortcut: TShortcut;
begin
  shortcut := Hotkey1.HotKey;

end;

end.
