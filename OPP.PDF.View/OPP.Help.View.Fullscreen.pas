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
  OPP.Help.System.Thread;

type

  TOPPHelpViewFullScreenStatus = record
    zoomMode: TdxPreviewZoomMode;
    zoomFactor: Integer;
  end;

  TOPPHelpViewSearchInstanciator = (siDocumentLoad, siPredicate);
  TOPPHelpViewOnStatusChanged = reference to procedure(AStatus: TOPPHelpViewFullScreenStatus);

  TOPPHelpViewFullScreen = class(TPanel)
  private
    fEventListeners: TList<IOPPHelpViewEventListener>;
    fHasLoadedDocument: Boolean;
    fIsFindPanelVisible: Boolean;
    fOnFindPanelVisiblityChange: TOPPHelpBooleanCompletion;
    fOnStatusChanged: TOPPHelpViewOnStatusChanged;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fSearchIsInProgress: Boolean;
    fSearchTimer: TTimer;
    function GetEventListeners(): TList<IOPPHelpViewEventListener>;
    function GetIsFindPanelVisible(): Boolean;
    function getPDFDocument(): TdxPDFDocument;
    function GetStatus: TOPPHelpViewFullScreenStatus;
    function GetZoomFactor: Integer;
    procedure DoSearchIfPossible(AInstanciator: TOPPHelpViewSearchInstanciator);
    procedure OnHideFindPanelEvent(Sender: TObject);
    procedure OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure OnShowFindPanelEvent(Sender: TObject);
    procedure onTimerTick(Sender: TObject);
    procedure SetHasLoadedDocument(AHasLoadedDocument: Boolean);
    procedure SetIsFindPanelVisible(AValue: Boolean);
    procedure SetZoomFactor(AValue: Integer);
    procedure SetSearchIsInProgress(const Value: Boolean);

    property EventListeners: TList<IOPPHelpViewEventListener> read GetEventListeners;
    property HasLoadedDocument: Boolean read fHasLoadedDocument write SetHasLoadedDocument;
    property pdfDocument: TdxPDFDocument read getPDFDocument;
    property SearchIsInProgress: Boolean read fSearchIsInProgress write SetSearchIsInProgress;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);

    function searchWork(Arg: Integer): Integer;

    procedure FitPageWidth;
    procedure FitPageHeight;
    procedure OnZoomFactorChanged(Sender: TObject);

    property IsFindPanelVisible: Boolean read GetIsFindPanelVisible write SetIsFindPanelVisible;
    property OnStatusChanged: TOPPHelpViewOnStatusChanged read fOnStatusChanged write fOnStatusChanged;
    property zoomFactor: Integer read GetZoomFactor write SetZoomFactor;
    property OnFindPanelVisibilityChange: TOPPHelpBooleanCompletion read fOnFindPanelVisiblityChange write fOnFindPanelVisiblityChange;
    property Status: TOPPHelpViewFullScreenStatus read GetStatus;
  end;

const
  kEventFlowName: String = 'PDFViewer';

implementation

uses System.SysUtils,
  OPP.Help.Log, OPP.Help.System.Types,
  OPP.Help.View.Helper,
  Vcl.Forms, AsyncCalls;

resourcestring
  SWarningPredicateIsNotDefined = 'Predicate is not defined';
  SWarningDocumentIsNotLoaded = 'Document is not loaded';
  SWarningSearchIsStillInProgress = 'Search is still in progress';
  SWarningStreamWasNotDefined = 'Stream was not defined';

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
  fSearchTimer.Interval := 100;
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
  DoSearchIfPossible(siPredicate);
end;

procedure TOPPHelpViewFullScreen.SetHasLoadedDocument(AHasLoadedDocument: Boolean);
begin
  fHasLoadedDocument := AHasLoadedDocument;
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
    eventLogger.Warning(SWarningStreamWasNotDefined, kEventFlowName);
  end;

  for fListener in EventListeners do
  begin
    if assigned(fListener) then
      fListener.LoadContentFinished;
  end;
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

procedure TOPPHelpViewFullScreen.DoSearchIfPossible(AInstanciator: TOPPHelpViewSearchInstanciator);
begin

  if fSearchIsInProgress then
  begin
    eventLogger.Warning(SWarningSearchIsStillInProgress, kEventFlowName);
    exit;
  end;

  if not HasLoadedDocument then
  begin
    eventLogger.Warning(SWarningDocumentIsNotLoaded, kEventFlowName);
    exit;
  end;

  if not assigned(fPredicate) then
  begin
    eventLogger.Warning(SWarningPredicateIsNotDefined, kEventFlowName);
    exit;
  end;

  searchWork(0);
  // AsyncCalls.AsyncCall(searchWork, 0).Sync;
end;

function TOPPHelpViewFullScreen.searchWork(Arg: Integer): Integer;
begin
  self.SearchIsInProgress := true;

  fPDFViewer.RunPredicate(fPredicate,
    procedure(AResult: Integer)
    begin
      self.SearchIsInProgress := false;
    end);

  result := 0;
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

procedure TOPPHelpViewFullScreen.SetSearchIsInProgress(const Value: Boolean);
var
  fListener: IOPPHelpViewEventListener;
begin

  fSearchIsInProgress := Value;
  fSearchTimer.Enabled := fSearchIsInProgress;

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
