unit OPP.Help.View.Fullscreen;

interface

uses
  System.Classes,
  System.Generics.Collections,
  WinAPI.Windows,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  dxPDFViewer, dxPDFDocument, dxCustomPreview,

  OPP.Help.Interfaces,
  OPP.Help.Predicate,
  OPP.Help.System.References,
  OPP.Help.PreviewSettings,

  {resources}
  dxPDFViewerDialogsStrs,
  dxCore,

  {print}
  dxPSdxPDFViewerLnk, dxPSCore;

type
  TOPPHelpViewFullScreenStatus = record
    zoomMode: TdxPreviewZoomMode;
    zoomFactor: Integer;
  end;

  TOPPPDFViewerOnScrollEvent = reference to function(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;

  TOPPPdfViewer = class(TdxPDFViewer)
  private
    fOnScrollEvent: TOPPPDFViewerOnScrollEvent;
  public
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    property onScrollEvent: TOPPPDFViewerOnScrollEvent read fOnScrollEvent write fOnScrollEvent;
  end;

  TOPPHelpViewSearchInstanciator = (siDocumentLoad, siPredicate);
  TOPPHelpViewOnStatusChanged = reference to procedure(AStatus: TOPPHelpViewFullScreenStatus);
  TOPPNavigatorConstraint = (ncCanGoFirstPage, ncCanGoPreviousPage, ncCanGoNextPage, ncCanGoLastPage);
  TOPPNavigatorConstraints = set of TOPPNavigatorConstraint;

  IOPPNavigator = interface;
  TOPPNavigatorStatusChangedCompletion = reference to procedure(ANavigator: IOPPNavigator);

  IOPPNavigator = interface
    procedure GotoFirstPage();
    procedure GotoPreviousPage();
    procedure GotoNextPage();
    procedure GotoLastPage();
    function NavigatorConstraints(): TOPPNavigatorConstraints;
    function GetPageIndex(): Integer;
    procedure SetPageIndex(AValue: Integer);
    function PagesCount(): Integer;
    procedure SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);

    property PageIndex: Integer read GetPageIndex write SetPageIndex;
  end;

  TOPPHelpViewFullScreen = class(TPanel, IOPPNavigator)
  private
    fEventListeners: TList<IOPPHelpViewEventListener>;
    fHasLoadedDocument: Boolean;
    fIsFindPanelVisible: Boolean;
    fNavigatorStatusChangesCompletion: TOPPNavigatorStatusChangedCompletion;
    fOnFindPanelVisiblityChange: TOPPHelpBooleanCompletion;
    fOnStatusChanged: TOPPHelpViewOnStatusChanged;
    fPDFViewer: TOPPPdfViewer;
    fPredicate: TOPPHelpPredicate;
    fSearchIsInProgress: Boolean;
    fScrollingType: TOPPHelpScrollingType;
    procedure DoSearchIfPossible(AInstanciator: TOPPHelpViewSearchInstanciator);
    function GetEventListeners(): TList<IOPPHelpViewEventListener>;
    function GetIsFindPanelVisible(): Boolean;
    function getPDFDocument(): TdxPDFDocument;
    function GetStatus: TOPPHelpViewFullScreenStatus;
    procedure OnHideFindPanelEvent(Sender: TObject);
    procedure OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    function OnPDFViewerScrollEvent(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
    procedure OnSelectedPageChanged(Sender: TObject; APageIndex: Integer);
    procedure OnShowFindPanelEvent(Sender: TObject);
    procedure SetHasLoadedDocument(AHasLoadedDocument: Boolean);
    procedure SetIsFindPanelVisible(AValue: Boolean);
    procedure SetSearchIsInProgress(const Value: Boolean);
    function GetZoomFactor: Integer;
    procedure SetZoomFactor(const Value: Integer);
    procedure SetScrollingType(const Value: TOPPHelpScrollingType);
    property EventListeners: TList<IOPPHelpViewEventListener> read GetEventListeners;
    property HasLoadedDocument: Boolean read fHasLoadedDocument write SetHasLoadedDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    property SearchIsInProgress: Boolean read fSearchIsInProgress write SetSearchIsInProgress;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure FitPageHeight;
    procedure FitPageWidth;
    procedure FitTwoColumns;
    procedure FitCustom(AFactor: Integer);
    function GetPageIndex(): Integer;
    procedure GotoFirstPage();
    procedure GotoLastPage();
    procedure GotoNextPage();
    procedure GotoPreviousPage();
    procedure LoadContent(AStream: TMemoryStream);
    procedure LoadDefaultResource(AResourceName: String = '');
    function NavigatorConstraints(): TOPPNavigatorConstraints;
    procedure OnZoomFactorChanged(Sender: TObject);
    function PagesCount(): Integer;
    procedure PrintCurrentPage(APrinterComponent: TCustomdxComponentPrinter);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure searchWork();
    procedure SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);
    procedure SetPageIndex(AValue: Integer);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure ShowPrintDialog(APrinterComponent: TCustomdxComponentPrinter);
    property IsFindPanelVisible: Boolean read GetIsFindPanelVisible write SetIsFindPanelVisible;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property Status: TOPPHelpViewFullScreenStatus read GetStatus;
    property zoomFactor: Integer read GetZoomFactor write SetZoomFactor;
    property ScrollingType: TOPPHelpScrollingType read fScrollingType write SetScrollingType default stLines;

    property OnFindPanelVisibilityChange: TOPPHelpBooleanCompletion read fOnFindPanelVisiblityChange write fOnFindPanelVisiblityChange;
    property OnStatusChanged: TOPPHelpViewOnStatusChanged read fOnStatusChanged write fOnStatusChanged;
  end;

const
  kContext: String = 'PDFViewer';

implementation

uses System.SysUtils,
  OPP.Help.Log, OPP.Help.System.Types,
  OPP.Help.View.Helper,
  {customization}
  cxControls,
  {print customization}
  dxPSGlbl, dxPrnDlg;

{$R 'OPPHelpPreviewCustom.res' 'OPPHelpPreviewCustom.rc'}

resourcestring
  SEventPrinterComponentWasNotDefined = 'Printer component was not defined';
  SWarningPredicateIsNotDefined = 'Predicate is not defined';
  SWarningDocumentIsNotLoaded = 'Document is not loaded';
  SWarningSearchIsStillInProgress = 'Search is still in progress';
  SWarningStreamWasNotDefined = 'Stream was not defined';

constructor TOPPHelpViewFullScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.BevelOuter := bvNone;

  fPDFViewer := TOPPPdfViewer.Create(self);
  fPDFViewer.parent := self;
  fPDFViewer.Align := alClient;
  fPDFViewer.OptionsZoom.zoomMode := pzmPages;
  fPDFViewer.OnDocumentLoaded := OnPDFViewer1DocumentLoaded;
  fPDFViewer.onScrollEvent := OnPDFViewerScrollEvent;

  fPDFViewer.OnHideFindPanel := OnHideFindPanelEvent;
  fPDFViewer.OnShowFindPanel := OnShowFindPanelEvent;
  fPDFViewer.OnSelectedPageChanged := OnSelectedPageChanged;
  fPDFViewer.OnZoomFactorChanged := OnZoomFactorChanged;

  fHasLoadedDocument := false;
  fSearchIsInProgress := false;

  fEventListeners := TList<IOPPHelpViewEventListener>.Create();
end;

destructor TOPPHelpViewFullScreen.Destroy;
begin
  fEventListeners.Free;

  inherited;
end;

procedure TOPPHelpViewFullScreen.addStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  EventListeners.add(AListener);
end;

procedure TOPPHelpViewFullScreen.DoSearchIfPossible(AInstanciator: TOPPHelpViewSearchInstanciator);
begin
  if fSearchIsInProgress then
  begin
    eventLogger.Warning(SWarningSearchIsStillInProgress, kContext);
    exit;
  end;

  if not HasLoadedDocument then
  begin
    eventLogger.Warning(SWarningDocumentIsNotLoaded, kContext);
    exit;
  end;

  if not assigned(fPredicate) then
  begin
    eventLogger.Warning(SWarningPredicateIsNotDefined, kContext);
    exit;
  end;

  searchWork();
end;

procedure TOPPHelpViewFullScreen.FitCustom(AFactor: Integer);
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmNone;
  fPDFViewer.OptionsZoom.zoomFactor := AFactor;
end;

procedure TOPPHelpViewFullScreen.FitPageHeight;
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmPages;
end;

procedure TOPPHelpViewFullScreen.FitPageWidth;
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmPageWidth;
end;

procedure TOPPHelpViewFullScreen.FitTwoColumns;
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmNone;
  fPDFViewer.OptionsZoom.zoomFactor := 81;
end;

function TOPPHelpViewFullScreen.GetEventListeners: TList<IOPPHelpViewEventListener>;
begin
  if not assigned(fEventListeners) then
    fEventListeners := TList<IOPPHelpViewEventListener>.Create();
  result := fEventListeners;
end;

function TOPPHelpViewFullScreen.GetIsFindPanelVisible(): Boolean;
begin
  result := fPDFViewer.IsFindPanelVisible;
end;

function TOPPHelpViewFullScreen.GetPageIndex: Integer;
begin
  result := 0;
  if assigned(fPDFViewer) then
    result := fPDFViewer.CurrentPageIndex;
end;

function TOPPHelpViewFullScreen.getPDFDocument(): TdxPDFDocument;
begin
  result := fPDFViewer.Document
end;

function TOPPHelpViewFullScreen.GetZoomFactor: Integer;
begin
  result := fPDFViewer.OptionsZoom.zoomFactor;
end;

procedure TOPPHelpViewFullScreen.GotoFirstPage;
begin
  PageIndex := 0;
end;

procedure TOPPHelpViewFullScreen.GotoLastPage;
begin
  PageIndex := (fPDFViewer.PageCount - 1);
end;

procedure TOPPHelpViewFullScreen.GotoNextPage;
begin
  PageIndex := (fPDFViewer.CurrentPageIndex + 1);
end;

procedure TOPPHelpViewFullScreen.GotoPreviousPage;
begin
  PageIndex := (fPDFViewer.CurrentPageIndex - 1);
end;

procedure TOPPHelpViewFullScreen.LoadContent(AStream: TMemoryStream);
var
  fListener: IOPPHelpViewEventListener;
begin

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.LoadContentStarted;
  end;

  if assigned(AStream) then
  begin
    fPDFViewer.LoadFromStream(AStream);
  end else begin
    eventLogger.Warning(SWarningStreamWasNotDefined, kContext);
  end;

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.LoadContentFinished;
  end;

  if assigned(fNavigatorStatusChangesCompletion) then
  begin
    fNavigatorStatusChangesCompletion(self);
  end;
end;

function TOPPHelpViewFullScreen.NavigatorConstraints: TOPPNavigatorConstraints;
begin
  result := [];
  if not assigned(fPDFViewer) then
    exit;

  if fPDFViewer.CurrentPageIndex <> 0 then
  begin
    result := result + [ncCanGoFirstPage];
    result := result + [ncCanGoPreviousPage];
  end;

  if fPDFViewer.CurrentPageIndex < (fPDFViewer.PageCount - 1) then
  begin
    result := result + [ncCanGoNextPage];
    result := result + [ncCanGoLastPage];
  end;
end;

procedure TOPPHelpViewFullScreen.OnHideFindPanelEvent(Sender: TObject);
begin
  if assigned(fOnFindPanelVisiblityChange) then
    fOnFindPanelVisiblityChange(false);
end;

procedure TOPPHelpViewFullScreen.OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  HasLoadedDocument := true;
  DoSearchIfPossible(siDocumentLoad);
end;

function TOPPHelpViewFullScreen.OnPDFViewerScrollEvent(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  cnt: Integer;
begin
  result := false;
  case fScrollingType of
    stLines:
      begin
        result := true;
      end;
    stPages:
      begin
        if assigned(fPDFViewer) then
        begin
          cnt := (-1) * Round(Extended(WheelDelta) / WHEEL_DELTA);
          fPDFViewer.CurrentPageIndex := fPDFViewer.CurrentPageIndex + cnt;
        end;
      end;
  end;
end;

procedure TOPPHelpViewFullScreen.OnSelectedPageChanged(Sender: TObject; APageIndex: Integer);
begin
  if assigned(fNavigatorStatusChangesCompletion) then
    fNavigatorStatusChangesCompletion(self);
end;

procedure TOPPHelpViewFullScreen.OnZoomFactorChanged(Sender: TObject);
begin
  if assigned(fOnStatusChanged) then
    fOnStatusChanged(Status);
end;

procedure TOPPHelpViewFullScreen.OnShowFindPanelEvent(Sender: TObject);
begin
  if assigned(fOnFindPanelVisiblityChange) then
    fOnFindPanelVisiblityChange(true);
end;

function TOPPHelpViewFullScreen.PagesCount: Integer;
begin
  result := 0;
  if assigned(fPDFViewer) then
    result := fPDFViewer.PageCount;
end;

procedure TOPPHelpViewFullScreen.PrintCurrentPage(APrinterComponent: TCustomdxComponentPrinter);
var
  lnk: TdxPDFViewerReportLink;
  pn: TdxPageNumbers;
  pages: TIntegers;
begin

  if not assigned(APrinterComponent) then
  begin
    eventLogger.Error(SEventPrinterComponentWasNotDefined, kContext);
    exit;
  end;

  SetLength(pages, 1);
  pages[0] := (fPDFViewer.CurrentPageIndex + 1);

  lnk := TdxPDFViewerReportLink.Create(self);
  try
    lnk.ComponentPrinter := APrinterComponent;
    lnk.Component := fPDFViewer;
    lnk.PrintPagesEx(pages, TdxPageNumbers.pnAll, 1, false);
  finally
    lnk.Free;
  end;
end;

procedure TOPPHelpViewFullScreen.removeStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  EventListeners.Remove(AListener);
end;

procedure TOPPHelpViewFullScreen.searchWork();
begin
  self.SearchIsInProgress := true;
  fPDFViewer.RunPredicate(fPredicate, 0,
    procedure(AResult: TOPPHelpViewPredicateExecutionResult; ALevel: Integer)
    begin
      self.SearchIsInProgress := false;
    end);
end;

procedure TOPPHelpViewFullScreen.SetHasLoadedDocument(AHasLoadedDocument: Boolean);
begin
  fHasLoadedDocument := AHasLoadedDocument;
end;

procedure TOPPHelpViewFullScreen.SetIsFindPanelVisible(AValue: Boolean);
begin
  if AValue then
    fPDFViewer.ShowFindPanel
  else
    fPDFViewer.HideFindPanel;
end;

procedure TOPPHelpViewFullScreen.SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);
begin
  fNavigatorStatusChangesCompletion := ACompletion;
end;

procedure TOPPHelpViewFullScreen.SetPageIndex(AValue: Integer);
begin
  if (AValue < (fPDFViewer.PageCount - 1)) or (AValue >= 0) then
  begin
    fPDFViewer.CurrentPageIndex := AValue;
  end;

  if assigned(fNavigatorStatusChangesCompletion) then
  begin
    fNavigatorStatusChangesCompletion(self);
  end;
end;

procedure TOPPHelpViewFullScreen.setPredicate(const APredicate: TOPPHelpPredicate);
begin
  fPredicate := APredicate;
  DoSearchIfPossible(siPredicate);
end;

procedure TOPPHelpViewFullScreen.SetScrollingType(const Value: TOPPHelpScrollingType);
begin
  fScrollingType := Value;
end;

procedure TOPPHelpViewFullScreen.SetSearchIsInProgress(const Value: Boolean);
var
  fListener: IOPPHelpViewEventListener;
begin

  fSearchIsInProgress := Value;

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
    begin
      if fSearchIsInProgress then
        fListener.SearchStarted
      else
        fListener.SearchEnded;
    end;
  end;
end;

procedure TOPPHelpViewFullScreen.SetZoomFactor(const Value: Integer);
begin
  fPDFViewer.OptionsZoom.zoomFactor := Value;
end;

procedure TOPPHelpViewFullScreen.ShowPrintDialog(APrinterComponent: TCustomdxComponentPrinter);
var
  lnk: TdxPDFViewerReportLink;
begin

  if not assigned(APrinterComponent) then
  begin
    eventLogger.Error(SEventPrinterComponentWasNotDefined, kContext);
    exit;
  end;

  lnk := TdxPDFViewerReportLink.Create(self);
  try
    lnk.ComponentPrinter := APrinterComponent;
    lnk.Component := fPDFViewer;
    lnk.Print(true);
  finally
    lnk.Free;
  end;
end;

procedure TOPPHelpViewFullScreen.LoadDefaultResource(AResourceName: String = '');
var
  stream: TResourceStream;
  fResourceName: String;
begin
  if Length(AResourceName) = 0 then
    fResourceName := 'RC_PDF_TITLE'
  else
    fResourceName := AResourceName;

  if (FindResource(hInstance, PChar(fResourceName), RT_RCDATA) = 0) then
  begin
    eventLogger.Error(Format('Resource ''%s'' not found', [fResourceName]));
    exit;
  end;

  try
    stream := TResourceStream.Create(hInstance, fResourceName, RT_RCDATA);
    try
      fPDFViewer.LoadFromStream(stream);
      fPDFViewer.OptionsZoom.zoomMode := pzmPageWidth;
    finally
      stream.Free;
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

function TOPPHelpViewFullScreen.GetStatus: TOPPHelpViewFullScreenStatus;
var
  CurrentPageIndex: Integer;
begin

  CurrentPageIndex := fPDFViewer.CurrentPageIndex;

  result.zoomMode := fPDFViewer.OptionsZoom.zoomMode;
  result.zoomFactor := fPDFViewer.OptionsZoom.zoomFactor;

{$REGION 'the fix for DevEx feature: it is changing preview when zoom mode was changed. updated view presents another page, so, use sleep, goto page that was visible before'}
  Sleep(20);
  if (CurrentPageIndex > 0) then
  begin
    fPDFViewer.CurrentPageIndex := (CurrentPageIndex - 1);
    Sleep(20);
    fPDFViewer.CurrentPageIndex := CurrentPageIndex;
  end;
{$ENDREGION}
end;

{ TOPPPdfViewer }

function TOPPPdfViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  ftobeHandled: Boolean;
begin
  ftobeHandled := true;
  if not assigned(fOnScrollEvent) then
  begin
    ftobeHandled := true;
  end else begin
    ftobeHandled := fOnScrollEvent(Shift, WheelDelta, MousePos);
  end;

  if (ftobeHandled) then
  begin
    result := inherited DoMouseWheel(Shift, WheelDelta, MousePos)
  end else begin
    result := true;
  end;

end;

initialization

cxSetResourceString(@sdxPDFViewerFindPanelFindCaption, 'Найти');
cxSetResourceString(@sdxPDFViewerFindPanelNextButtonCaption, 'Следующий');
cxSetResourceString(@sdxPDFViewerFindPanelPreviousButtonCaption, 'Предыдущий');
cxSetResourceString(@sdxPDFViewerTextSearchingNoMatchesFoundMessage, 'Поиск закончен. Ничего больше не найдено.');
cxSetResourceString(@sdxPDFViewerTextSearchingCompleteMessage, 'Поиск закончен.');
cxSetResourceString(@sdxPDFViewerFindPanelPopupMenuCaseSensitive, 'Зависимость от регистра');
cxSetResourceString(@sdxPDFViewerFindPanelPopupMenuWholeWords, 'Слова целиком');

finalization

end.
