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

  TOPPHelpViewFullScreen = class(TPanel)
  private
    fStream: TMemoryStream;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fHasLoadedDocument: Boolean;
    fSearchIsInProgress: Boolean;
    fSearchTimer: TTimer;
    fEventListeners: TList<IOPPHelpViewEventListener>;
    fOnStatusChanged: TOPPHelpViewOnStatusChanged;
    function GetZoomFactor: Integer;
    procedure SetZoomFactor(AValue: Integer);
    function GetStatus: TOPPHelpViewFullScreenStatus;
    function getPDFDocument(): TdxPDFDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    procedure onPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure doSearchIfPossible(instanciator: TOPPHelpViewSearchInstanciator);
    procedure onTimerTick(Sender: TObject);
    function GetEventListeners(): TList<IOPPHelpViewEventListener>;
    procedure setHasLoadedDocument(AHasLoadedDocument: Boolean);
    property HasLoadedDocument: Boolean read fHasLoadedDocument write setHasLoadedDocument;
    property EventListeners: TList<IOPPHelpViewEventListener> read GetEventListeners;
    property Status: TOPPHelpViewFullScreenStatus read GetStatus;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure loadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);

    procedure loadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure loadWorkEnded(AResult: Integer);

    procedure searchWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure searchWorkEnded(AResult: Integer);
    procedure searchWork(APredicate: TOPPHelpPredicate; onFinish: TOPPHelpThreadOnFinish);
    procedure execute(APredicate: TOPPHelpPredicate; completion: TOPPHelpViewPredicateExecutionCompletion);

    procedure FitPageWidth;
    procedure FitPageHeight;
    procedure OnZoomFactorChanged(Sender: TObject);

    property OnStatusChanged: TOPPHelpViewOnStatusChanged read fOnStatusChanged write fOnStatusChanged;
    property ZoomFactor: Integer read GetZoomFactor write SetZoomFactor;
  end;

implementation

uses System.SysUtils, OPP.Help.Log;

constructor TOPPHelpViewFullScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.BevelOuter := bvNone;

  fPDFViewer := TdxPDFViewer.Create(self);
  fPDFViewer.parent := self;
  fPDFViewer.Align := alClient;
  fPDFViewer.OptionsZoom.zoomMode := pzmPageWidth;
  fPDFViewer.OnDocumentLoaded := onPDFViewer1DocumentLoaded;
  fPDFViewer.OnZoomFactorChanged := OnZoomFactorChanged;

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
  doSearchIfPossible(siPredicate);
end;

procedure TOPPHelpViewFullScreen.setHasLoadedDocument(AHasLoadedDocument: Boolean);
begin
  fHasLoadedDocument := AHasLoadedDocument;
end;

procedure TOPPHelpViewFullScreen.loadContent(AStream: TMemoryStream);
begin

  fStream := AStream;

  TThread.Synchronize(nil,
    procedure()
    begin
      loadWorkStarted(loadWorkEnded);
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

procedure TOPPHelpViewFullScreen.onPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  HasLoadedDocument := true;
  doSearchIfPossible(siDocumentLoad);
end;

function TOPPHelpViewFullScreen.getPDFDocument(): TdxPDFDocument;
begin
  result := fPDFViewer.Document
end;

procedure TOPPHelpViewFullScreen.doSearchIfPossible(instanciator: TOPPHelpViewSearchInstanciator);
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

  execute(fPredicate,
    procedure(AResult: Integer)
    begin
      onFinish(AResult);
    end);
end;

procedure TOPPHelpViewFullScreen.loadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
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

procedure TOPPHelpViewFullScreen.loadWorkEnded(AResult: Integer);
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

end.
