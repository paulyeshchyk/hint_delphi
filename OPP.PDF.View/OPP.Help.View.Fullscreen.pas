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

  TOPPNavigatorConstraint = (ncCanGoFirstPage, ncCanGoPreviousPage, ncCanGoNextPage, ncCanGoLastPage);
  TOPPNavigatorConstraints = set of TOPPNavigatorConstraint;

  IOPPNavigator = interface;
  TOPPNavigatorStatusChangedCompletion = reference to procedure(ANavigator: IOPPNavigator);

  IOPPNavigator = interface
    procedure GotoFirstPage();
    procedure GotoPreviousPage();
    procedure GotoNextPage();
    procedure GotoLastPage();
    procedure GotoCustomPage(APage: Integer);
    function NavigatorConstraints(): TOPPNavigatorConstraints;
    function PageIndex(): Integer;
    function PagesCount(): Integer;
    procedure SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);
  end;

  TOPPHelpViewFullScreen = class(TPanel, IOPPNavigator)
  private
    fEventListeners: TList<IOPPHelpViewEventListener>;
    fHasLoadedDocument: Boolean;
    fIsFindPanelVisible: Boolean;
    fOnFindPanelVisiblityChange: TOPPHelpBooleanCompletion;
    fOnStatusChanged: TOPPHelpViewOnStatusChanged;
    fPDFViewer: TdxPDFViewer;
    fPredicate: TOPPHelpPredicate;
    fSearchIsInProgress: Boolean;
    fNavigatorStatusChangesCompletion: TOPPNavigatorStatusChangedCompletion;
    function GetEventListeners(): TList<IOPPHelpViewEventListener>;
    function GetIsFindPanelVisible(): Boolean;
    function getPDFDocument(): TdxPDFDocument;
    function GetStatus: TOPPHelpViewFullScreenStatus;
    function GetZoomFactor: Integer;
    procedure DoSearchIfPossible(AInstanciator: TOPPHelpViewSearchInstanciator);
    procedure OnHideFindPanelEvent(Sender: TObject);
    procedure OnPDFViewer1DocumentLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure OnShowFindPanelEvent(Sender: TObject);
    procedure OnSelectedPageChanged(Sender: TObject; APageIndex: Integer);
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

    procedure searchWork();

    procedure FitPageWidth;
    procedure FitPageHeight;
    procedure OnZoomFactorChanged(Sender: TObject);

    { IOPPNavigator }
    procedure GotoFirstPage();
    procedure GotoPreviousPage();
    procedure GotoNextPage();
    procedure GotoLastPage();
    procedure GotoCustomPage(APage: Integer);
    function NavigatorConstraints(): TOPPNavigatorConstraints;
    procedure SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);
    function PageIndex(): Integer;
    function PagesCount(): Integer;

    {}
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
  OPP.Help.View.Helper;

resourcestring
  SWarningPredicateIsNotDefined = 'Predicate is not defined';
  SWarningDocumentIsNotLoaded = 'Document is not loaded';
  SWarningSearchIsStillInProgress = 'Search is still in progress';
  SWarningStreamWasNotDefined = 'Stream was not defined';

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
  fPDFViewer.OnSelectedPageChanged := OnSelectedPageChanged;

  fHasLoadedDocument := false;
  fSearchIsInProgress := false;

  fEventListeners := TList<IOPPHelpViewEventListener>.Create();
end;

destructor TOPPHelpViewFullScreen.Destroy;
begin
  fEventListeners.Free;
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

  if assigned(fNavigatorStatusChangesCompletion) then
  begin
    fNavigatorStatusChangesCompletion(self);
  end;
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

  searchWork();

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

function TOPPHelpViewFullScreen.PageIndex: Integer;
begin
  result := 0;
  if assigned(fPDFViewer) then
    result := fPDFViewer.CurrentPageIndex;
end;

function TOPPHelpViewFullScreen.PagesCount: Integer;
begin
  result := 0;
  if assigned(fPDFViewer) then
    result := fPDFViewer.PageCount;
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

procedure TOPPHelpViewFullScreen.GotoCustomPage(APage: Integer);
begin
  if (APage < (fPDFViewer.PageCount - 1)) or (APage >= 0) then
  begin
    fPDFViewer.CurrentPageIndex := APage;
  end;

  if assigned(fNavigatorStatusChangesCompletion) then
  begin
    fNavigatorStatusChangesCompletion(self);
  end;
end;

procedure TOPPHelpViewFullScreen.GotoFirstPage;
begin
  GotoCustomPage(0);
end;

procedure TOPPHelpViewFullScreen.GotoLastPage;
begin
  GotoCustomPage((fPDFViewer.PageCount - 1));
end;

procedure TOPPHelpViewFullScreen.GotoNextPage;
begin
  GotoCustomPage(fPDFViewer.CurrentPageIndex + 1);
end;

procedure TOPPHelpViewFullScreen.GotoPreviousPage;
begin
  GotoCustomPage(fPDFViewer.CurrentPageIndex - 1);
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

procedure TOPPHelpViewFullScreen.SetNavigatorStatusChangesCompletion(ACompletion: TOPPNavigatorStatusChangedCompletion);
begin
  fNavigatorStatusChangesCompletion := ACompletion;
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

procedure TOPPHelpViewFullScreen.OnSelectedPageChanged(Sender: TObject; APageIndex: Integer);
begin
  if assigned(fNavigatorStatusChangesCompletion) then
  begin
    fNavigatorStatusChangesCompletion(self);
  end;
end;

procedure TOPPHelpViewFullScreen.OnShowFindPanelEvent(Sender: TObject);
begin
  if assigned(fOnFindPanelVisiblityChange) then
    fOnFindPanelVisiblityChange(true);
end;

end.
