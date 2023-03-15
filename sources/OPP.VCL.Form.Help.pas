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
  dxBarBuiltInMenu, dxCustomPreview, dxPDFViewer, VCL.ComCtrls, VCL.WinXCtrls, VCL.ExtCtrls,
  OPP.VCL.Form.Help.Thread;

type

  TOPPFormHelp = class(TForm)
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton1: TdxBarButton;
    ProgressBar1: TProgressBar;
    dxPDFViewer1: TdxPDFViewer;
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dxPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fPDFViewer: TdxPDFViewer;
    fThread: TOPPFormHelpThread;
    fHasContent: Bool;
    fStream: TMemoryStream;
    fTextForSearch: String;
    fProgress: Integer;
    function getPDFDocument(): TdxPDFDocument;
    procedure setStream(AStream: TMemoryStream);
    procedure setTextForSearch(AText: String);
    procedure doSearchIfPossible;
    procedure SearchJobDone;
    procedure SearchJob;
    procedure pdfChangedThePage;
    function getPDFViewer: TdxPDFViewer;
    function doIncrementPosition(): Integer;
  public
    { Public declarations }
    property stream: TMemoryStream read fStream write setStream;
    property textForSearch: String read fTextForSearch write setTextForSearch;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    property pdfViewer: TdxPDFViewer read getPDFViewer;

    procedure openPage(AIndex: Integer);
  end;

var
  OPPFormHelp: TOPPFormHelp;

implementation

{$R *.dfm}

uses
  System.UITypes;

procedure TOPPFormHelp.FormCreate(Sender: TObject);
var
  semiTransparent: TAlphaColorRec;
begin
  fHasContent := false;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  fProgress := 0;
  Timer1.Enabled := false;
end;

procedure TOPPFormHelp.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TOPPFormHelp.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Position := doIncrementPosition;
end;

procedure TOPPFormHelp.dxPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  fHasContent := true;
  doSearchIfPossible;
end;

function TOPPFormHelp.doIncrementPosition: Integer;
begin
  fProgress := fProgress + 1;
  if fProgress >= 100 then
    fProgress := 0;
  result := fProgress;
end;

function TOPPFormHelp.getPDFViewer: TdxPDFViewer;
begin
  result := dxPDFViewer1;
end;

procedure TOPPFormHelp.doSearchIfPossible;
var
  fThread: TOPPFormHelpThread;
begin
  if not fHasContent then
    exit;

  if Length(fTextForSearch) = 0 then
    exit;
  fThread := TOPPFormHelpThread.Create(self.SearchJob, self.SearchJobDone);
  fThread.resume;
end;

procedure TOPPFormHelp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.pdfViewer.SelPageIndex := 0;
  self.pdfViewer.ChangePage(pdfChangedThePage);
end;

procedure TOPPFormHelp.SearchJob;
var
  searchResult: TdxPDFDocumentTextSearchResult;
begin
  Timer1.Enabled := true;
  searchResult := pdfDocument.FindText(fTextForSearch);
  self.pdfViewer.SelPageIndex := searchResult.range.pageIndex;
  self.pdfViewer.ChangePage(pdfChangedThePage);
  Timer1.Enabled := false;
  ProgressBar1.Position := 0;
end;

procedure TOPPFormHelp.pdfChangedThePage;
begin

end;

procedure TOPPFormHelp.SearchJobDone;
begin
  //
end;

function TOPPFormHelp.getPDFDocument(): TdxPDFDocument;
begin
  if Assigned(self.pdfViewer) then begin
    result := self.pdfViewer.Document;
  end else begin
    result := nil;
  end;
end;

procedure TOPPFormHelp.setTextForSearch(AText: String);
begin
  fTextForSearch := AText;
  doSearchIfPossible;
end;

procedure TOPPFormHelp.setStream(AStream: TMemoryStream);
begin

  fStream := AStream;

  if not Assigned(fStream) and not Assigned(pdfViewer) then
    exit;
  //
  pdfViewer.LoadFromStream(fStream);
end;

procedure TOPPFormHelp.openPage(AIndex: Integer);
begin
  self.pdfViewer.SelPageIndex := AIndex;
  self.pdfViewer.ChangePage(pdfChangedThePage);

end;

end.
