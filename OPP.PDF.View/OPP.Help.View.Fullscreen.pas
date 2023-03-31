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

  TOPPHelpViewSearchInstanciator = (siDocumentLoad, siPredicate);

  TOPPHelpViewFullScreen = class(TPanel)
  private
    fStream: TMemoryStream;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fHasLoadedDocument: Boolean;
    fSearchIsInProgress: Boolean;
    fSearchTimer: TTimer;
    fEventListeners: TList<IOPPHelpViewEventListener>;
    function getPDFDocument(): TdxPDFDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    procedure onPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure doSearchIfPossible(instanciator: TOPPHelpViewSearchInstanciator);
    procedure onTimerTick(Sender: TObject);
    function GetEventListeners():TList<IOPPHelpViewEventListener>;
    procedure setHasLoadedDocument(AHasLoadedDocument: Boolean);
    property HasLoadedDocument: Boolean read fHasLoadedDocument write setHasLoadedDocument;
    property EventListeners: TList<IOPPHelpViewEventListener> read GetEventListeners;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure reloadControls;

    procedure triggerFindPanel;

    procedure loadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);

    procedure loadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure loadWorkEnded(AResult: Integer);

    procedure searchWorkStarted(onFinish: TOPPHelpThreadOnFinish);
    procedure searchWorkEnded(AResult: Integer);
    procedure searchWork();

  end;

implementation

uses System.SysUtils;

constructor TOPPHelpViewFullScreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.BevelOuter := bvNone;

  fPDFViewer := TdxPDFViewer.Create(self);
  fPDFViewer.parent := self;
  fPDFViewer.Align := alClient;
  fPDFViewer.OptionsZoom.ZoomMode := pzmPageWidth;
  fPDFViewer.OnDocumentLoaded := onPDFViewer1DocumentLoaded;

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

procedure TOPPHelpViewFullScreen.reloadControls;
begin
  //
end;

procedure TOPPHelpViewFullScreen.triggerFindPanel;
begin
  fPDFViewer.ShowFindPanel;
end;

function TOPPHelpViewFullScreen.GetEventListeners: TList<IOPPHelpViewEventListener>;
begin
  if not assigned( fEventListeners) then
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
  reloadControls;
end;

procedure TOPPHelpViewFullScreen.loadContent(AStream: TMemoryStream);
begin

  fStream := AStream;

  TThread.Synchronize(nil,
    procedure()
    begin
      loadWorkStarted(loadWorkEnded);
    end);

  // TOPPSystemThread.Create(loadWorkStarted, loadWorkEnded);
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

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.SearchStarted;
  end;

  if not assigned(fPredicate) then
  begin
    if assigned(onFinish) then
      onFinish(0);
    exit;
  end;

  searchWork();

  if assigned(onFinish) then
    onFinish(0);
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

procedure TOPPHelpViewFullScreen.searchWork();
var
  fSearchResult: TdxPDFDocumentTextSearchResult;
begin
  fSearchTimer.Enabled := true;

  case fPredicate.keywordType of
    ktSearch:
      begin
        fSearchResult := pdfDocument.FindText(fPredicate.value);
        fPDFViewer.CurrentPageIndex := fSearchResult.range.pageIndex;
      end;
    ktBookmark:
      begin
        //
      end;
    ktPage:
      begin
        fPDFViewer.CurrentPageIndex := StrToInt(fPredicate.value);
      end;
    ktAny:
      begin
        //
      end;
  end;

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

end.
