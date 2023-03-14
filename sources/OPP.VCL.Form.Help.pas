unit OPP.VCL.Form.Help;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, dxBar, cxClasses,
  cxPC, dxDockControl, dxDockPanel, cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxPDFDocument,
  dxBarBuiltInMenu, dxCustomPreview, dxPDFViewer;

type
  TOPPFormHelp = class(TForm)
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton1: TdxBarButton;
    dxPDFViewer1: TdxPDFViewer;
    procedure proc;
    procedure dxPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OPPFormHelp: TOPPFormHelp;

implementation

{$R *.dfm}

procedure TOPPFormHelp.proc;
begin
  //
end;

procedure TOPPFormHelp.dxPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
var
  searchResult: TdxPDFDocumentTextSearchResult;
begin

  searchResult := Sender.FindText('Справочник содержит коды и наименования Общероссийского классификатора видов экономической');

  dxPDFViewer1.SelPageIndex := searchResult.range.pageIndex;
  dxPDFViewer1.ChangePage(proc);
end;

procedure TOPPFormHelp.FormCreate(Sender: TObject);
begin
  dxPDFViewer1.LoadFromFile('manual.pdf');
end;

end.
