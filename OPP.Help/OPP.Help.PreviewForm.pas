unit OPP.Help.PreviewForm;

interface

uses
  cxBarEditItem, cxClasses, cxContainer, cxControls, cxEdit, cxGraphics,
  cxButtonEdit, cxSpinEdit,
  cxTrackBar,
  cxLookAndFeelPainters, cxLookAndFeels, cxPC, cxProgressBar, cxStyles, dxBar,
  dxDockControl, dxDockPanel, dxStatusBar,

  OPP.Help.Interfaces, OPP.Help.Predicate,
  OPP.Help.View.Fullscreen,
  OPP.Help.System.Stream,
  OPP.Help.System.Messaging,

  System.Classes, System.SysUtils, System.Variants,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.AppEvnts,
  Winapi.Messages, Winapi.Windows,
  Vcl.ActnList, System.Actions, Vcl.StdActns, dxDBSparkline, dxNumericWheelPicker,
  dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkroom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin,
  dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue,
  dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White,
  dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp,
  dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, dxPrnDev, dxPrnDlg, dxPSGlbl,
  dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv, dxPSPrVwRibbon, dxPScxPageControlProducer,
  dxPScxEditorProducers, dxPScxExtEditorProducers, dxPSCore, cxTextEdit, cxPropertiesStore, dxPScxGridLnk,
  dxPScxGridLayoutViewLnk;

type

  TOPPHelpPreviewFormState = (fsFormCreated, fsLoadContentStarted, fsLoadContentFinished, fsHandlingMessage, fsSearchStarted, fsSearchProgressing, fsSearchFinished, fsIdle);

  TOPPHelpPreviewFormStateHelper = record helper for TOPPHelpPreviewFormState
  public
    function asString(): String;
  end;

  TOPPHelpPreviewForm = class(TForm, IOPPHelpViewEventListener, IOPPHelpShortcutViewer)
    actionFitPageCustom: TAction;
    actionFitPageHeight: TAction;
    actionFitPageWidth: TAction;
    actionFitTwoPages: TAction;
    actionGotoContents: TAction;
    actionGotoFirstPage: TAction;
    actionGotoLastPage: TAction;
    actionGotoNextPage: TAction;
    actionGotoPreviousPage: TAction;
    actionGotoTerms: TAction;
    actionHide: TAction;
    ActionList1: TActionList;
    actionPrint: TAction;
    actionPrintDialog: TAction;
    actionToggleFindPanel: TAction;
    ApplicationEvents1: TApplicationEvents;
    buttonFirstPage: TdxBarLargeButton;
    buttonLastPage: TdxBarLargeButton;
    buttonNextPage: TdxBarLargeButton;
    buttonPreviousPage: TdxBarLargeButton;
    buttonQuickNavigation: TdxBarSubItem;
    cxBarEditItem1: TcxBarEditItem;
    cxBarEditItem2: TcxBarEditItem;
    cxBarEditItem3: TcxBarEditItem;
    cxBarEditItem4: TcxBarEditItem;
    cxBarEditItem5: TcxBarEditItem;
    cxEditGotoCustomPage: TcxBarEditItem;
    cxProgressBar1: TcxProgressBar;
    dropdownPageSelectionEdit: TcxBarEditItem;
    dxBarButton1: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarButton12: TdxBarButton;
    dxBarButton13: TdxBarButton;
    dxBarButton14: TdxBarButton;
    dxBarButton15: TdxBarButton;
    dxBarButton16: TdxBarButton;
    dxBarButton17: TdxBarButton;
    dxBarButton18: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarButtonExit: TdxBarButton;
    dxBarDockControl1: TdxBarDockControl;
    dxBarDockControl2: TdxBarDockControl;
    dxBarDockControl3: TdxBarDockControl;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarLargeButton4: TdxBarLargeButton;
    dxBarLargeButton5: TdxBarLargeButton;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarLargeButton7: TdxBarLargeButton;
    dxBarLargeButton8: TdxBarLargeButton;
    dxBarLargeButton9: TdxBarLargeButton;
    dxBarManager1: TdxBarManager;
    barNavigator: TdxBar;
    dxBarManager1Bar5: TdxBar;
    barJump: TdxBar;
    dxBarSeparator1: TdxBarSeparator;
    dxBarSeparator2: TdxBarSeparator;
    dxBarSeparator3: TdxBarSeparator;
    dxBarSeparator4: TdxBarSeparator;
    dxBarSeparator5: TdxBarSeparator;
    dxBarSeparator6: TdxBarSeparator;
    dxBarSeparator7: TdxBarSeparator;
    dxBarSeparator8: TdxBarSeparator;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarSubItem4: TdxBarSubItem;
    dxBarSubItem5: TdxBarSubItem;
    dxBarSubItem6: TdxBarSubItem;
    dxBarSubItem7: TdxBarSubItem;
    dxBarSubItem8: TdxBarSubItem;
    dxComponentPrinter1: TdxComponentPrinter;
    dxDockingManager1: TdxDockingManager;
    dxStatusBar1: TdxStatusBar;
    dxStatusBar1Container0: TdxStatusBarContainerControl;
    FileExit1: TFileExit;
    findPaneTogglerButton: TdxBarButton;
    findPaneTogglerButton1: TdxBarLargeButton;
    fitActualSizeButton: TdxBarButton;
    fitPageWidthButton: TdxBarButton;
    TrayIcon1: TTrayIcon;
    zoomMenuButton: TdxBarSubItem;
    zoomValueEdit: TcxBarEditItem;
    dxBarButton19: TdxBarButton;
    dxBarSeparator9: TdxBarSeparator;
    barPrint: TdxBar;
    barExit: TdxBar;
    barZoom: TdxBar;
    dxBarSubItem9: TdxBarSubItem;
    dxBarLargeButton10: TdxBarLargeButton;
    actionZoomIncrease: TAction;
    actionZoomDecrease: TAction;
    dxBarLargeButton11: TdxBarLargeButton;
    dxBarLargeButton12: TdxBarLargeButton;
    procedure actionFitPageCustomExecute(Sender: TObject);
    procedure actionFitPageHeightExecute(Sender: TObject);
    procedure actionFitPageWidthExecute(Sender: TObject);
    procedure actionFitTwoPagesExecute(Sender: TObject);
    procedure actionGotoContentsExecute(Sender: TObject);
    procedure actionGotoFirstPageExecute(Sender: TObject);
    procedure actionGotoLastPageExecute(Sender: TObject);
    procedure actionGotoNextPageExecute(Sender: TObject);
    procedure actionGotoPreviousPageExecute(Sender: TObject);
    procedure actionGotoTermsExecute(Sender: TObject);
    procedure actionHideExecute(Sender: TObject);
    procedure actionPrintDialogExecute(Sender: TObject);
    procedure actionPrintExecute(Sender: TObject);
    procedure actionToggleFindPanelExecute(Sender: TObject);
    procedure actionZoomDecreaseExecute(Sender: TObject);
    procedure actionZoomIncreaseExecute(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure cxBarEditItem4Change(Sender: TObject);
    procedure cxEditGotoCustomPageChange(Sender: TObject);
    procedure cxEditGotoCustomPagePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure dxBarButtonExitClick(Sender: TObject);
    procedure dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure zoomValueEditChange(Sender: TObject);
  private
    fCurrentPredicate: TOPPHelpPredicate;
    fCurrentState: TOPPHelpPreviewFormState;
    fNavigator: IOPPNavigator;
    oppHelpView: TOPPHelpViewFullScreen;
    reloadIsInProgress: Boolean;
    function GetInfoPanel: TdxStatusBarPanel;
    procedure HandleWMCOPYMessage(APtr: Pointer; ASize: NativeInt);
    procedure LoadContentFinished();
    { --- }
    procedure LoadContentStarted();
    procedure OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
    procedure ParsePredicate(const AStream: TReadOnlyMemoryStream; out resultPredicate: TOPPHelpPredicate);
    procedure ReloadNavigationPanel(ANavigator: IOPPNavigator);
    procedure RestoreFromBackground();
    procedure SearchEnded();
    procedure SearchProgress();
    procedure SearchStarted();
    { --- }

    procedure SendToBackground();
    procedure SetCurrentState(ACurrentState: TOPPHelpPreviewFormState);
    property currentState: TOPPHelpPreviewFormState read fCurrentState write SetCurrentState;
    property InfoPanel: TdxStatusBarPanel read GetInfoPanel;
    property Navigator: IOPPNavigator read fNavigator;
  protected
    procedure OnMessageWMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure OnMessageWMOPPPredicate(var Msg: TMessage); message WM_OPPPredicate;
    procedure OnMessageWMUserZoom(var Msg: TMessage); message WM_OPPZoom;
    procedure OnMessageWMUserZoomFit(var Msg: TMessage); message WM_OPPZoomFit;
  public
    { Public declarations }
    function GetContainerClassName: String;
    procedure PresentModal();
    procedure runPredicate(const APredicate: TOPPHelpPredicate);
  end;

var
  OPPHelpPreviewForm: TOPPHelpPreviewForm;

implementation

{$R *.dfm}

uses
  dxCustomPreview,
  OPP.Help.Shortcut.Server,
  OPP.Help.View.ZoomSettings,
  OPP.Help.Log, OPP.Help.System.Error,
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Types,

  AsyncCalls;

const
  kGulfstreamContentsPageIndex = '2';
  kGulfstreamTermsPageIndex = '1056';
  kZoomTwoPagesFactor = 81;
  kZoomDefaultIncrement = 5;

resourcestring
  SRC_PDF_TITLE = 'RC_PDF_TITLE';
  SEvebtPostedSuccessResult = 'Posted success result';
  SEventStateTemplate = 'State: %s';
  SEventParsedPredicateTemplate = 'Parsed predicate: %s';
  SEventFinishedPDFLoadTemplate = 'Finished PDF load: %s';
  SEventStartedPDFLoadTemplate = 'Started PDF load: %s';
  SFlowReceivedMessage = 'Received Message';
  SScalingTemplate = 'Масштаб %d %%';
  SStatusIdle = 'Idle';
  SStatusSearchFinished = ''; // Search finished
  SStatusSearchProgressing = 'Поиск ...'; // Search is progressing
  SStatusSearchStarted = 'Начат поиск ...'; // Search started
  SStatusHandling = 'Handling';
  SStatusLoadContentStarted = 'Началась загрузка'; // Load content started
  SStatusLoadContentFinished = 'Загрузка завершена'; // Load content finished
  SStatusFormCreated = ''; // Form created

procedure TOPPHelpPreviewForm.actionFitPageCustomExecute(Sender: TObject);
var
  specialZoom: TOPPHelpPreviewZoomForm;
begin
  specialZoom := TOPPHelpPreviewZoomForm.Create(self);
  try
    specialZoom.zoomValue := oppHelpView.ZoomFactor;
    specialZoom.ShowModal;
  finally
    specialZoom.Free;
  end;
end;

procedure TOPPHelpPreviewForm.actionFitPageHeightExecute(Sender: TObject);
begin
  oppHelpView.FitPageHeight();
end;

procedure TOPPHelpPreviewForm.actionFitPageWidthExecute(Sender: TObject);
begin
  oppHelpView.FitPageWidth();
end;

procedure TOPPHelpPreviewForm.actionFitTwoPagesExecute(Sender: TObject);
begin
  oppHelpView.ZoomFactor := kZoomTwoPagesFactor;
end;

procedure TOPPHelpPreviewForm.actionGotoContentsExecute(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin
  fPredicate := TOPPHelpPredicate.Create;
  try
    fPredicate.keywordType := ktPage;
    fPredicate.value := kGulfstreamContentsPageIndex;
    oppHelpView.setPredicate(fPredicate);
  finally
    fPredicate.Free;
  end;
end;

procedure TOPPHelpPreviewForm.actionGotoFirstPageExecute(Sender: TObject);
begin
  if assigned(Navigator) then
    Navigator.GotoFirstPage;
end;

procedure TOPPHelpPreviewForm.actionGotoLastPageExecute(Sender: TObject);
begin
  if assigned(Navigator) then
    Navigator.GotoLastPage;
end;

procedure TOPPHelpPreviewForm.actionGotoNextPageExecute(Sender: TObject);
begin
  if assigned(Navigator) then
    Navigator.GotoNextPage;
end;

procedure TOPPHelpPreviewForm.actionGotoPreviousPageExecute(Sender: TObject);
begin
  if assigned(Navigator) then
    Navigator.GotoPreviousPage;
end;

procedure TOPPHelpPreviewForm.actionGotoTermsExecute(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin
  fPredicate := TOPPHelpPredicate.Create;
  try
    fPredicate.keywordType := ktPage;
    fPredicate.value := kGulfstreamTermsPageIndex;
    oppHelpView.setPredicate(fPredicate);
  finally
    fPredicate.Free;
  end;
end;

procedure TOPPHelpPreviewForm.actionHideExecute(Sender: TObject);
begin
  Close;
  // Application.Minimize;
end;

procedure TOPPHelpPreviewForm.actionPrintDialogExecute(Sender: TObject);
begin
  oppHelpView.ShowPrintDialog(dxComponentPrinter1);
end;

procedure TOPPHelpPreviewForm.actionPrintExecute(Sender: TObject);
begin
  oppHelpView.PrintCurrentPage(dxComponentPrinter1);
end;

procedure TOPPHelpPreviewForm.actionToggleFindPanelExecute(Sender: TObject);
begin
  oppHelpView.IsFindPanelVisible := not oppHelpView.IsFindPanelVisible;
end;

procedure TOPPHelpPreviewForm.actionZoomDecreaseExecute(Sender: TObject);
begin
  oppHelpView.ZoomFactor := oppHelpView.ZoomFactor - kZoomDefaultIncrement;
end;

procedure TOPPHelpPreviewForm.actionZoomIncreaseExecute(Sender: TObject);
begin
  oppHelpView.ZoomFactor := oppHelpView.ZoomFactor + kZoomDefaultIncrement;
end;

procedure TOPPHelpPreviewForm.ApplicationEvents1Minimize(Sender: TObject);
begin
  self.SendToBackground();
  TrayIcon1.Visible := true;
end;

procedure TOPPHelpPreviewForm.ApplicationEvents1Restore(Sender: TObject);
begin
  TrayIcon1.Visible := false;
end;

procedure TOPPHelpPreviewForm.cxBarEditItem4Change(Sender: TObject);
begin
  oppHelpView.ZoomFactor := Integer(cxBarEditItem4.EditValue);
end;

procedure TOPPHelpPreviewForm.cxEditGotoCustomPageChange(Sender: TObject);
var
  fIndex: Integer;
  fValue: Variant;
begin

  if reloadIsInProgress then
    exit;

  fValue := TcxBarEditItem(Sender).EditValue;
  fIndex := StrToIntDef(Trim(VarToStr(fValue)), 1);
  oppHelpView.PageIndex := (fIndex - 1);

end;

procedure TOPPHelpPreviewForm.cxEditGotoCustomPagePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
var
  fIndex, fIndexToCheck: Integer;
  fCurrentIndex, fPagesCount: Integer;
begin
  fCurrentIndex := oppHelpView.GetPageIndex();
  fPagesCount := oppHelpView.PagesCount();

  fIndex := StrToIntDef(Trim(VarToStr(DisplayValue)), -1);
  fIndexToCheck := fIndex - 1;
  if fIndexToCheck < 0 then
  begin
    DisplayValue := (fCurrentIndex + 1);
  end
  else if fIndexToCheck > fPagesCount then
  begin
    DisplayValue := (fCurrentIndex + 1);
  end else begin
    DisplayValue := fIndex;
  end;
end;

procedure TOPPHelpPreviewForm.dxBarButtonExitClick(Sender: TObject);
begin
  self.Close();
end;

procedure TOPPHelpPreviewForm.dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := false;
end;

procedure TOPPHelpPreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oppHelpView.removeStateChangeListener(self);
end;

procedure TOPPHelpPreviewForm.FormCreate(Sender: TObject);
begin
  reloadIsInProgress := false;
  self.currentState := fsFormCreated;

  oppHelpView := TOPPHelpViewFullScreen.Create(self);
  oppHelpView.Parent := self;
  oppHelpView.Align := alClient;
  oppHelpView.OnStatusChanged := self.OnViewStatusChanged;
  self.OnViewStatusChanged(oppHelpView.Status);

  oppHelpView.loadDefaultResource(SRC_PDF_TITLE);

  oppHelpView.OnFindPanelVisibilityChange := procedure(AValue: Boolean)
    begin
      findPaneTogglerButton.Down := AValue;
    end;

  fNavigator := oppHelpView as IOPPNavigator;
  self.Navigator.SetNavigatorStatusChangesCompletion(
    procedure(ANavigator: IOPPNavigator)
    begin
      if assigned(ANavigator) then
      begin
        self.ReloadNavigationPanel(ANavigator);
      end;
    end);

  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);
  ReloadNavigationPanel(nil);
end;

procedure TOPPHelpPreviewForm.FormDestroy(Sender: TObject);
begin
  if assigned(fNavigator) then
  begin
    fNavigator.SetNavigatorStatusChangesCompletion(nil);
  end;
end;

function TOPPHelpPreviewForm.GetContainerClassName: String;
begin
  Result := self.className;
end;

function TOPPHelpPreviewForm.GetInfoPanel: TdxStatusBarPanel;
begin
  Result := dxStatusBar1.Panels[1];
end;

procedure TOPPHelpPreviewForm.HandleWMCOPYMessage(APtr: Pointer; ASize: NativeInt);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
begin
  eventLogger.Flow(SFlowReceivedMessage, OPP.Help.View.Fullscreen.kEventFlowName);

  currentState := fsHandlingMessage;

  fNotificationStream := TReadOnlyMemoryStream.Create(APtr, ASize);
  try
    ParsePredicate(fNotificationStream, fPredicate);
    try
      runPredicate(fPredicate);
    finally
      fPredicate.Free;
    end;
  finally
    fNotificationStream.Free;
  end;
end;

{ --------- }

procedure TOPPHelpPreviewForm.LoadContentFinished;
begin
  currentState := fsLoadContentFinished;
end;

procedure TOPPHelpPreviewForm.LoadContentStarted();
begin
  currentState := fsLoadContentStarted;
end;

procedure TOPPHelpPreviewForm.OnMessageWMCopyData(var Msg: TWMCopyData);
var
  fNotificationStream: TReadOnlyMemoryStream;
begin

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  try
    try
      ParsePredicate(fNotificationStream, fCurrentPredicate);
    except
      on e: Exception do
      begin
        e.Log();
      end;
    end;
  finally
    fNotificationStream.Free;
  end;

  PostMessage(self.Handle, WM_OPPPredicate, 0, 0);

  eventLogger.Flow(SEvebtPostedSuccessResult, kEventFlowName);

  Msg.Result := SMessageResultSuccess;

end;

procedure TOPPHelpPreviewForm.OnMessageWMOPPPredicate(var Msg: TMessage);
begin
  Msg.Result := SMessageResultSuccess;

  runPredicate(fCurrentPredicate);

end;

procedure TOPPHelpPreviewForm.OnMessageWMUserZoom(var Msg: TMessage);
begin
  oppHelpView.ZoomFactor := Msg.WParam;
end;

procedure TOPPHelpPreviewForm.OnMessageWMUserZoomFit(var Msg: TMessage);
begin
  case Msg.WParam of
    WM_OPPZoomFitWParamHeight:
      begin
        oppHelpView.FitPageHeight;
      end;
    WM_OPPZoomFitWParamWidth:
      begin
        oppHelpView.FitPageWidth;
      end;
    WM_OPPZoomFitWParamTwoColumns:
      begin
        oppHelpView.ZoomFactor := 81;
      end;
  end;
  Msg.Result := oppHelpView.ZoomFactor;
  //
end;

procedure TOPPHelpPreviewForm.OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
begin
  fitActualSizeButton.Down := (AStatus.zoomMode = TdxPreviewZoomMode.pzmPageWidth);
  zoomValueEdit.EditValue := AStatus.ZoomFactor;
  zoomMenuButton.Caption := Format(SScalingTemplate, [AStatus.ZoomFactor]);
end;

procedure TOPPHelpPreviewForm.ParsePredicate(const AStream: TReadOnlyMemoryStream; out resultPredicate: TOPPHelpPredicate);
begin
  resultPredicate := TOPPHelpPredicate.Create();
  resultPredicate.readFromStream(AStream, true);
  eventLogger.Flow(Format(SEventParsedPredicateTemplate, [resultPredicate.asString]), OPP.Help.View.Fullscreen.kEventFlowName)
end;

procedure TOPPHelpPreviewForm.PresentModal;
begin
  ShowModal;
end;

procedure TOPPHelpPreviewForm.ReloadNavigationPanel(ANavigator: IOPPNavigator);
var
  AConstraints: TOPPNavigatorConstraints;
  aPageToGo: Variant;
begin
  reloadIsInProgress := true;

  if assigned(ANavigator) then
  begin
    AConstraints := ANavigator.NavigatorConstraints;
    aPageToGo := (ANavigator.GetPageIndex + 1);
  end else begin
    AConstraints := [];
    aPageToGo := null;
  end;

  actionZoomIncrease.Enabled := (AConstraints <> []);
  actionZoomDecrease.Enabled := (AConstraints <> []);
  actionFitTwoPages.Enabled := (AConstraints <> []);
  actionFitPageCustom.Enabled := (AConstraints <> []);
  actionFitPageWidth.Enabled := (AConstraints <> []);
  actionFitPageHeight.Enabled := (AConstraints <> []);
  cxEditGotoCustomPage.Enabled := (AConstraints <> []);
  dropdownPageSelectionEdit.Enabled := (AConstraints <> []);
  actionToggleFindPanel.Enabled := (AConstraints <> []);
  buttonQuickNavigation.Enabled := (AConstraints <> []);

  actionPrint.Enabled := (AConstraints <> []);
  actionPrintDialog.Enabled := (AConstraints <> []);

  actionGotoContents.Enabled := (AConstraints <> []);
  actionGotoTerms.Enabled := (AConstraints <> []);
  actionGotoFirstPage.Enabled := ncCanGoFirstPage in AConstraints;
  actionGotoPreviousPage.Enabled := ncCanGoPreviousPage in AConstraints;
  actionGotoNextPage.Enabled := ncCanGoNextPage in AConstraints;
  actionGotoLastPage.Enabled := ncCanGoLastPage in AConstraints;

  cxEditGotoCustomPage.Enabled := (AConstraints <> []);
  cxEditGotoCustomPage.Properties.LockUpdate(true);
  cxEditGotoCustomPage.EditValue := aPageToGo;
  cxEditGotoCustomPage.Properties.LockUpdate(false);
  reloadIsInProgress := false;
end;

procedure TOPPHelpPreviewForm.RestoreFromBackground;
begin
  Application.Restore;
  Application.BringToFront();
  self.show();
  self.WindowState := TWindowState.wsNormal;
end;

procedure TOPPHelpPreviewForm.runPredicate(const APredicate: TOPPHelpPredicate);
begin
  eventLogger.Flow(Format(SEventStartedPDFLoadTemplate, [APredicate.filename]), OPP.Help.View.Fullscreen.kEventFlowName);
  helpShortcutServer.loadPDF(APredicate.filename,
    procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus)
    begin
      eventLogger.Flow(Format(SEventFinishedPDFLoadTemplate, [APredicate.filename]), OPP.Help.View.Fullscreen.kEventFlowName);
      if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssError then
        exit;

      try
        if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssCreated then
          oppHelpView.LoadContent(AStream);
      finally
        oppHelpView.setPredicate(APredicate);
      end;
    end);
end;

procedure TOPPHelpPreviewForm.SearchEnded();
begin
  currentState := fsSearchFinished;
end;

procedure TOPPHelpPreviewForm.SearchProgress();
begin
end;

procedure TOPPHelpPreviewForm.SearchStarted();
begin
  currentState := fsSearchStarted;
end;

procedure TOPPHelpPreviewForm.SendToBackground;
begin
  self.Hide();
  self.WindowState := TWindowState.wsMinimized;
end;

procedure TOPPHelpPreviewForm.SetCurrentState(ACurrentState: TOPPHelpPreviewFormState);
var
  fStateStr: String;
begin
  fCurrentState := ACurrentState;

  fStateStr := fCurrentState.asString;
  InfoPanel.Text := fStateStr;
  eventLogger.Flow(Format(SEventStateTemplate, [fStateStr]), kEventFlowName);
end;

procedure TOPPHelpPreviewForm.TrayIcon1Click(Sender: TObject);
begin
  self.RestoreFromBackground();
end;

procedure TOPPHelpPreviewForm.zoomValueEditChange(Sender: TObject);
begin
  oppHelpView.ZoomFactor := Integer(zoomValueEdit.EditValue);
end;

{ TOPPHelpPreviewFormStateHelper }

function TOPPHelpPreviewFormStateHelper.asString: String;
begin
  Result := '';
  case self of
    fsFormCreated:
      Result := SStatusFormCreated;
    fsLoadContentStarted:
      Result := SStatusLoadContentStarted;
    fsLoadContentFinished:
      Result := SStatusLoadContentFinished;
    fsHandlingMessage:
      Result := SStatusHandling;
    fsSearchStarted:
      Result := SStatusSearchStarted;
    fsSearchProgressing:
      Result := SStatusSearchProgressing;
    fsSearchFinished:
      Result := SStatusSearchFinished;
    fsIdle:
      Result := SStatusIdle;
  end;
end;

end.
