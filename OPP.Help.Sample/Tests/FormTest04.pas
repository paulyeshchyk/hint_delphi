unit FormTest04;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel,
  dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy,
  dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky,
  dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue,
  dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White,
  dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp,
  dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, cxPC, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, dxBar, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxDockPanel, dxDockControl,
  Datasnap.DBClient;

type
  TFormTest4 = class(TForm)
    dxDockSite2: TdxDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxDockPanel2: TdxDockPanel;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    dxBarDockControl1: TdxBarDockControl;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxDockPanel1: TdxDockPanel;
    cxGrid2: TcxGrid;
    cxGrid2DBTableView1: TcxGridDBTableView;
    cxGrid2Level1: TcxGridLevel;
    dxBarDockControl2: TdxBarDockControl;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarManager1Bar2: TdxBar;
    dxBarManager1Bar3: TdxBar;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    DataSource2: TDataSource;
    predicatesDataset: TClientDataSet;
    DataSource1: TDataSource;
    recordsDataset: TClientDataSet;
    procedure dxBarButton3Click(Sender: TObject);
    procedure cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest4: TFormTest4;

implementation

{$R *.dfm}

uses SampleFormHelper;

procedure TFormTest4.cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
  AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
var
  fValue: Variant;
begin

  if Sender.DataController.ItemCount = 0 then
    exit;
  // if Sender.DataController.FocusedRecordIndex < 0 then
  // exit;

  fValue := Sender.DataController.Values[Sender.DataController.FocusedRecordIndex, 0];
  TSampleFormHelper.loadPredicatesDataset(predicatesDataset, fValue);

end;

procedure TFormTest4.dxBarButton3Click(Sender: TObject);
begin
  TSampleFormHelper.makeRecordsDataset(recordsDataset);
  TSampleFormHelper.makePredicatesDataset(predicatesDataset);

  cxGrid1DBTableView1.DataController.CreateAllItems();
  cxGrid1DBTableView1.ApplyBestFit();

  cxGrid2DBTableView1.DataController.CreateAllItems();
  cxGrid2DBTableView1.ApplyBestFit();

end;

end.
