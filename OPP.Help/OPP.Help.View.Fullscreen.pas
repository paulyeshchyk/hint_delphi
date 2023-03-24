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

  OPP.Help.Nonatomic,
  OPP.Help.Predicate,
  OPP.Help.System.Thread,
  OPP.Help.View,

  OPP.Help.System.Stream;

type

  TOPPHelpViewFullScreen = class(TPanel)
  private
    fStream: TMemoryStream;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fHasLoadedContent: Boolean;
    fSearchTimer: TTimer;
    fEventListeners: TList<IOPPHelpViewEventListener>;
    function getPDFDocument(): TdxPDFDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    procedure onPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure doSearchIfPossible();
    procedure onTimerTick(Sender: TObject);
    procedure setHasLoadedContent(AHasLoadedContent: Boolean);
    property HasLoadedContent: Boolean read fHasLoadedContent write setHasLoadedContent;
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

  end;

procedure Register;

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

  fHasLoadedContent := false;

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

procedure TOPPHelpViewFullScreen.setPredicate(const APredicate: TOPPHelpPredicate);
begin
  fPredicate := APredicate.copy();
  doSearchIfPossible;
end;

procedure TOPPHelpViewFullScreen.setHasLoadedContent(AHasLoadedContent: Boolean);
begin
  fHasLoadedContent := AHasLoadedContent;
  doSearchIfPossible;
  reloadControls;
end;

procedure TOPPHelpViewFullScreen.loadContent(AStream: TMemoryStream);
begin

  fStream := AStream;
  TOPPSystemThread.Create(loadWorkStarted, loadWorkEnded);
end;

procedure TOPPHelpViewFullScreen.onTimerTick(Sender: TObject);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in fEventListeners do
    fListener.SearchProgress();
end;

procedure TOPPHelpViewFullScreen.addStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  fEventListeners.add(AListener);
end;

procedure TOPPHelpViewFullScreen.removeStateChangeListener(AListener: IOPPHelpViewEventListener);
begin
  fEventListeners.Remove(AListener);
end;

procedure TOPPHelpViewFullScreen.onPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
begin
  HasLoadedContent := true;
  doSearchIfPossible;
end;

function TOPPHelpViewFullScreen.getPDFDocument(): TdxPDFDocument;
begin
  result := fPDFViewer.Document
end;

procedure TOPPHelpViewFullScreen.doSearchIfPossible();
begin
  if not HasLoadedContent then
    exit;

  if not assigned(fPredicate) then
    exit;

  TOPPSystemThread.Create(searchWorkStarted, searchWorkEnded);
end;

// threads
procedure TOPPHelpViewFullScreen.searchWorkStarted(onFinish: TOPPHelpThreadOnFinish);
var
  fSearchResult: TdxPDFDocumentTextSearchResult;
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in fEventListeners do
    fListener.SearchStarted;

  if not assigned(fPredicate) then
  begin
    if assigned(onFinish) then
      onFinish(0);
    exit;
  end;

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

  if assigned(onFinish) then
    onFinish(0);
end;

procedure TOPPHelpViewFullScreen.searchWorkEnded(AResult: Integer);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in fEventListeners do
    fListener.SearchEnded;

  fSearchTimer.Enabled := false;
end;

procedure TOPPHelpViewFullScreen.loadWorkStarted(onFinish: TOPPHelpThreadOnFinish);
var
  fListener: IOPPHelpViewEventListener;
begin
  for fListener in fEventListeners do
    fListener.SearchStarted;

  fSearchTimer.Enabled := true;

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
  for fListener in fEventListeners do
    fListener.SearchEnded;

  fSearchTimer.Enabled := false;
end;

procedure Register;
begin
  RegisterComponents('OPPHelp', [TOPPHelpViewFullScreen])
end;

end.
