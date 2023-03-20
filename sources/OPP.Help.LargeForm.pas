unit OPP.Help.LargeForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel,
  dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle,
  dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast,
  dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky,
  dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins,
  dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray,
  dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful,
  dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters,
  dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint,
  dxSkinXmas2008Blue, dxBar, cxClasses, cxPC, dxDockControl, dxDockPanel,
  cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxPDFDocument,
  dxBarBuiltInMenu, dxCustomPreview, dxPDFViewer, Vcl.ComCtrls, Vcl.ExtCtrls,
  OPP.Help.Shortcut.Mapping, OPP.System.Thread;

type
  TOPPHelpLargeForm = class(TForm)
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
    fHasContent: Bool;
    fStream: TMemoryStream;
    fShortcutMap: TOPPHelpShortcutMap;
    fProgress: Integer;
    function getPDFDocument(): TdxPDFDocument;
    procedure setStream(AStream: TMemoryStream);
    procedure setShortcutMap(AMap: TOPPHelpShortcutMap);
    procedure doSearchIfPossible;
    procedure threadFinishedWork(AResult: Integer);
    procedure SearchJob(onFinish: TOPPHelpThreadOnFinish);
    procedure pdfChangedThePage;
    function getPDFViewer: TdxPDFViewer;
    function doIncrementPosition(): Integer;
  public
    { Public declarations }
    property stream: TMemoryStream read fStream write setStream;
    property shortcutMap: TOPPHelpShortcutMap read fShortcutMap write setShortcutMap;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    property pdfViewer: TdxPDFViewer read getPDFViewer;

    procedure openPage(AIndex: Integer);
  end;

var
  OPPHelpLargeForm: TOPPHelpLargeForm;

implementation

{$R *.dfm}

uses
  System.UITypes;

// file://docs/гольфстрим_руководство пользователя.pdf?page=1&text=

procedure TOPPHelpLargeForm.FormCreate(Sender: TObject);
begin
  fHasContent := false;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  fProgress := 0;
  Timer1.Enabled := false;
end;

procedure TOPPHelpLargeForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TOPPHelpLargeForm.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Position := doIncrementPosition;
end;

procedure TOPPHelpLargeForm.dxPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  fHasContent := true;
  doSearchIfPossible;
end;

function TOPPHelpLargeForm.doIncrementPosition: Integer;
begin
  fProgress := fProgress + 1;
  if fProgress >= 100 then
    fProgress := 0;
  result := fProgress;
end;

function TOPPHelpLargeForm.getPDFViewer: TdxPDFViewer;
begin
  result := dxPDFViewer1;
end;

procedure TOPPHelpLargeForm.doSearchIfPossible;
begin
  if not fHasContent then
    exit;
  if not assigned(shortcutMap) then
    exit;
  if Length(shortcutMap.SearchPattern) = 0 then
    exit;
  TOPPSystemThread.Create(SearchJob, threadFinishedWork);
end;

procedure TOPPHelpLargeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.pdfViewer.SelPageIndex := 0;
  self.pdfViewer.ChangePage(pdfChangedThePage);
end;

procedure TOPPHelpLargeForm.SearchJob(onFinish: TOPPHelpThreadOnFinish);
var
  searchResult: TdxPDFDocumentTextSearchResult;
begin
  Timer1.Enabled := true;

  searchResult := pdfDocument.FindText(shortcutMap.SearchPattern);
  self.pdfViewer.SelPageIndex := searchResult.range.pageIndex;
  self.pdfViewer.ChangePage(pdfChangedThePage);
  if assigned(onFinish) then
    onFinish(0);
end;

procedure TOPPHelpLargeForm.pdfChangedThePage;
begin
  //
end;

procedure TOPPHelpLargeForm.threadFinishedWork(AResult: Integer);
begin
  Timer1.Enabled := false;
  ProgressBar1.Position := 0;
end;

function TOPPHelpLargeForm.getPDFDocument(): TdxPDFDocument;
begin
  if assigned(self.pdfViewer) then
  begin
    result := self.pdfViewer.Document;
  end else begin
    result := nil;
  end;
end;

procedure TOPPHelpLargeForm.setShortcutMap(AMap: TOPPHelpShortcutMap);
begin
  fShortcutMap := AMap;
  doSearchIfPossible;
end;

procedure TOPPHelpLargeForm.setStream(AStream: TMemoryStream);
begin
  fStream := AStream;
  if not assigned(fStream) and not assigned(pdfViewer) then
    exit;
  //
  pdfViewer.LoadFromStream(fStream);
end;

procedure TOPPHelpLargeForm.openPage(AIndex: Integer);
begin
  self.pdfViewer.SelPageIndex := AIndex;
  self.pdfViewer.ChangePage(pdfChangedThePage);
end;

end.
