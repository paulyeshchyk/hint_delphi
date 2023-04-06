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
  OPP.Help.Nonatomic,
  OPP.Help.System.Thread;

type

  TOPPHelpViewFullScreenStatus = record
    zoomMode: TdxPreviewZoomMode;
    zoomFactor: Integer;
  end;

  TOPPHelpViewSearchInstanciator = (siDocumentLoad, siPredicate);
  TOPPHelpViewPredicateExecutionCompletion = reference to procedure(AResult: Integer);
  TOPPHelpViewOnStatusChanged = reference to procedure(AStatus: TOPPHelpViewFullScreenStatus);
  TOPPHelpViewOnFindPanelVisibilityChange = reference to procedure(AIsVisible: Boolean);

  TOPPHelpViewFullScreen = class(TPanel)
  private
    fEventListeners: TList<IOPPHelpViewEventListener>;
    fHasLoadedDocument: Boolean;
    fIsFindPanelVisible: Boolean;
    fOnFindPanelVisiblityChange: TOPPHelpViewOnFindPanelVisibilityChange;
    fOnStatusChanged: TOPPHelpViewOnStatusChanged;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fSearchIsInProgress: Boolean;
    fSearchTimer: TTimer;
    fStream: TMemoryStream;
    function GetEventListeners(): TList<IOPPHelpViewEventListener>;
    function GetIsFindPanelVisible(): Boolean;
    function getPDFDocument(): TdxPDFDocument;
    function GetStatus: TOPPHelpViewFullScreenStatus;
    function GetZoomFactor: Integer;
    procedure DoSearchIfPossible(instanciator: TOPPHelpViewSearchInstanciator);
    procedure OnHideFindPanelEvent(Sender: TObject);
    procedure OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure OnShowFindPanelEvent(Sender: TObject);
    procedure onTimerTick(Sender: TObject);
    procedure SetHasLoadedDocument(AHasLoadedDocument: Boolean);
    procedure SetIsFindPanelVisible(AValue: Boolean);
    procedure SetZoomFactor(AValue: Integer);
    property EventListeners: TList<IOPPHelpViewEventListener> read GetEventListeners;
    property HasLoadedDocument: Boolean read fHasLoadedDocument write SetHasLoadedDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure loadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);

    procedure LoadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure LoadWorkEnded(AResult: Integer);

    procedure searchWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure searchWorkEnded(AResult: Integer);
    procedure searchWork(APredicate: TOPPHelpPredicate; onFinish: TOPPHelpThreadOnFinish);
    procedure execute(APredicate: TOPPHelpPredicate; completion: TOPPHelpViewPredicateExecutionCompletion);

    procedure FitPageWidth;
    procedure FitPageHeight;
    procedure OnZoomFactorChanged(Sender: TObject);

    property IsFindPanelVisible: Boolean read GetIsFindPanelVisible write SetIsFindPanelVisible;
    property OnStatusChanged: TOPPHelpViewOnStatusChanged read fOnStatusChanged write fOnStatusChanged;
    property zoomFactor: Integer read GetZoomFactor write SetZoomFactor;
    property OnFindPanelVisibilityChange: TOPPHelpViewOnFindPanelVisibilityChange read fOnFindPanelVisiblityChange write fOnFindPanelVisiblityChange;
    property Status: TOPPHelpViewFullScreenStatus read GetStatus;
  end;
const kEventFlowName: String = 'PDFViewer';


implementation

uses System.SysUtils, OPP.Help.Log;

constructor TOPPHelpViewFullScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.BevelOuter := bvNone;

  fPDFViewer := TdxPDFViewer.Create(self);
  fPDFViewer.parent := self;
  fPDFViewer.Align := alClient;
  fPDFViewer.OptionsZoom.zoomMode := pzmPages;
  fPDFViewer.OnDocumentLoaded := OnPDFViewer1DocumentLoaded;
  fPDFViewer.OnZoomFactorChanged := OnZoomFactorChanged;
  fPDFViewer.OnHideFindPanel := OnHideFindPanelEvent;
  fPDFViewer.OnShowFindPanel := OnShowFindPanelEvent;

  fHasLoadedDocument := false;
  fSearchIsInProgress := false;

  fSearchTimer := TTimer.Create(self);
  fSearchTimer.Enabled := false;
  fSearchTimer.OnTimer := onTimerTick;

  fEventListeners := TList<IOPPHelpViewEventListener>.Create();
end;

destructor TOPPHelpViewFullScreen.Destroy;
begin
  fEventListeners.Free;
  fSearchTimer.Free;
  fPDFViewer.Free;

  inherited;
end;

function TOPPHelpViewFullScreen.GetEventListeners: TList<IOPPHelpViewEventListener>;
begin
  if not assigned(fEventListeners) then
    fEventListeners := TList<IOPPHelpViewEventListener>.Create();
  result := fEventListeners;
end;

procedure TOPPHelpViewFullScreen.setPredicate(const APredicate: TOPPHelpPredicate);
begin
  fPredicate := APredicate.copy();

  //TODO: Force update
  //fPDFViewer.CurrentPageIndex := 1;

  DoSearchIfPossible(siPredicate);
end;

procedure TOPPHelpViewFullScreen.SetHasLoadedDocument(AHasLoadedDocument: Boolean);
begin
  fHasLoadedDocument := AHasLoadedDocument;
end;

procedure TOPPHelpViewFullScreen.loadContent(AStream: TMemoryStream);
begin

  fStream := AStream;

  TThread.Synchronize(nil,
    procedure()
    begin
      LoadWorkStarted(LoadWorkEnded);
    end);
end;

procedure TOPPHelpViewFullScreen.onTimerTick(Sender: TObject);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in EventListeners do
    fListener.SearchProgress();
end;

procedure TOPPHelpViewFullScreen.addStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  EventListeners.add(AListener);
end;

procedure TOPPHelpViewFullScreen.removeStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  EventListeners.Remove(AListener);
end;

procedure TOPPHelpViewFullScreen.OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  HasLoadedDocument := true;
  DoSearchIfPossible(siDocumentLoad);
end;

function TOPPHelpViewFullScreen.getPDFDocument(): TdxPDFDocument;
begin
  result := fPDFViewer.Document
end;

procedure TOPPHelpViewFullScreen.DoSearchIfPossible(instanciator: TOPPHelpViewSearchInstanciator);
begin

  if fSearchIsInProgress then
    exit;

  if not HasLoadedDocument then
    exit;

  if not assigned(fPredicate) then
    exit;

  TThread.Synchronize(nil,
    procedure()
    begin
      searchWorkStarted(searchWorkEnded);
    end);

end;

// threads
procedure TOPPHelpViewFullScreen.searchWorkStarted(onFinish: TOPPHelpThreadOnFinish);
var
  fListener: IOPPHelpViewEventListener;
begin

  fSearchIsInProgress := true;
  fSearchTimer.Enabled := true;

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.SearchStarted;
  end;

  searchWork(fPredicate, onFinish);
end;

procedure TOPPHelpViewFullScreen.searchWorkEnded(AResult: Integer);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.SearchEnded;
  end;

  fSearchTimer.Enabled := false;
  fSearchIsInProgress := false;
end;

procedure TOPPHelpViewFullScreen.searchWork(APredicate: TOPPHelpPredicate; onFinish: TOPPHelpThreadOnFinish);
begin
  eventLogger.Flow('started searchWord', kEventFlowName);

  if not assigned(onFinish) then
  begin
    eventLogger.Error('onFinish is not defined');
    exit;
  end;

  if not assigned(APredicate) then
  begin
    eventLogger.Error('predicate is not defined');
    onFinish(-1);
    exit;
  end;

  //fPDFViewer.TextSearch.Clear;
  execute(fPredicate,
    procedure(AResult: Integer)
    begin
      onFinish(AResult);
    end);
end;

procedure TOPPHelpViewFullScreen.LoadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.LoadStarted;
  end;

  if not assigned(fStream) then
  begin
    if assigned(onFinish) then
      onFinish(0);
  end else begin
    fPDFViewer.LoadFromStream(fStream);
  end;
end;

procedure TOPPHelpViewFullScreen.LoadWorkEnded(AResult: Integer);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.SearchEnded;
  end;
end;

procedure TOPPHelpViewFullScreen.execute(APredicate: TOPPHelpPredicate; completion: TOPPHelpViewPredicateExecutionCompletion);
var
  fSearchResult: TdxPDFDocumentTextSearchResult;
  nested: TOPPHelpPredicate;
  fCurrentPageIndex: Integer;
begin
  case APredicate.keywordType of
    ktSearch:
      begin
        eventLogger.Flow(Format('executed ktSearch:%s', [APredicate.value]), kEventFlowName);
        fCurrentPageIndex := fPDFViewer.CurrentPageIndex;
        fSearchResult := pdfDocument.FindText(APredicate.value, TdxPDFDocumentTextSearchOptions.Default, fCurrentPageIndex);
        fPDFViewer.CurrentPageIndex := fSearchResult.range.pageIndex;
      end;
    ktBookmark:
      begin
        //
      end;
    ktPage:
      begin
        eventLogger.Flow(Format('executed ktPage:%s', [APredicate.value]), kEventFlowName);
        fPDFViewer.CurrentPageIndex := StrToInt(APredicate.value);
      end;
    ktAny:
      begin
        //
      end;
  end;

  for nested in APredicate.predicates do
  begin
    self.execute(nested,
      procedure(AResult: Integer)
      begin
      end);
  end;

  if assigned(completion) then
    completion(0);

end;

procedure TOPPHelpViewFullScreen.FitPageWidth;
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmPageWidth;
end;

procedure TOPPHelpViewFullScreen.FitPageHeight;
begin
  fPDFViewer.OptionsZoom.zoomMode := pzmPages;
end;

procedure TOPPHelpViewFullScreen.OnZoomFactorChanged(Sender: TObject);
begin
  if assigned(fOnStatusChanged) then
    fOnStatusChanged(Status);
end;

function TOPPHelpViewFullScreen.GetStatus: TOPPHelpViewFullScreenStatus;
begin
  result.zoomMode := fPDFViewer.OptionsZoom.zoomMode;
  result.zoomFactor := fPDFViewer.OptionsZoom.zoomFactor;
end;

function TOPPHelpViewFullScreen.GetZoomFactor: Integer;
begin
  result := fPDFViewer.OptionsZoom.zoomFactor;
end;

procedure TOPPHelpViewFullScreen.SetZoomFactor(AValue: Integer);
begin
  fPDFViewer.OptionsZoom.zoomFactor := Integer(AValue);
  if assigned(fOnStatusChanged) then
    fOnStatusChanged(Status);
end;

procedure TOPPHelpViewFullScreen.SetIsFindPanelVisible(AValue: Boolean);
begin
  if AValue then
    fPDFViewer.ShowFindPanel
  else
    fPDFViewer.HideFindPanel;
end;

function TOPPHelpViewFullScreen.GetIsFindPanelVisible(): Boolean;
begin
  result := fPDFViewer.IsFindPanelVisible;
end;

procedure TOPPHelpViewFullScreen.OnHideFindPanelEvent(Sender: TObject);
begin
  if assigned(fOnFindPanelVisiblityChange) then
    fOnFindPanelVisiblityChange(false);
end;

procedure TOPPHelpViewFullScreen.OnShowFindPanelEvent(Sender: TObject);
begin
  if assigned(fOnFindPanelVisiblityChange) then
    fOnFindPanelVisiblityChange(true);
end;

end.
