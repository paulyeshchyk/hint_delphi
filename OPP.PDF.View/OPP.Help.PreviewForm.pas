unit OPP.Help.PreviewForm;

interface

uses
  cxBarEditItem, cxClasses, cxContainer, cxControls, cxEdit, cxGraphics,
  cxButtonEdit, cxSpinEdit,
  cxTrackBar,
  cxLookAndFeelPainters, cxLookAndFeels, cxPC, cxProgressBar, cxStyles, dxBar,
  dxDockControl, dxDockPanel, dxStatusBar,

  OPP.Help.Interfaces, OPP.Help.Predicate,
  OPP.Help.View.CommandLine,
  OPP.Help.View.Fullscreen,
  OPP.Help.System.Stream,
  OPP.Help.System.Messaging,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Help.PreviewSettings,
  OPP.Help.System.Timer,
  OPP.Help.PreviewForm.LogObserver,

  System.Classes, System.SysUtils, System.Variants, System.Generics.Collections,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.AppEvnts,
  Winapi.Messages, Winapi.Windows,
  Vcl.ActnList, System.Actions, Vcl.StdActns, dxDBSparkline, dxNumericWheelPicker,
  dxPrnDev, dxPrnDlg, dxPSGlbl,
  dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv, dxPSPrVwRibbon, dxPScxPageControlProducer,
  dxPScxEditorProducers, dxPScxExtEditorProducers, dxPSCore, cxTextEdit, cxPropertiesStore, dxPScxGridLnk,
  dxPScxGridLayoutViewLnk, cxMemo;

type

  TOPPHelpPreviewFormState = (fsFormCreated, fsLoadContentStarted, fsLoadContentFinished, fsHandlingMessage, fsSearchStarted, fsSearchProgressing, fsSearchFinished, fsIdle);

  TOPPHelpPreviewFormStateHelper = record helper for TOPPHelpPreviewFormState
  public
    function asString(): String;
  end;

  TOPPHelpPreviewNavigationPanelInfo = record
    constraints: TOPPNavigatorConstraints;
    gotopage: Variant;
  end;

  TOPPHelpPreviewNavigationPanelInfoHelper = record helper for TOPPHelpPreviewNavigationPanelInfo
    function hasConstraints: Boolean;
  end;

  TOPPHelpPreviewForm = class(TForm, IOPPHelpViewEventListener, IOPPHelpShortcutViewer)
    actionFitPageCustom: TAction;
    actionFitPageHeight: TAction;
    actionFitPageWidth: TAction;
    actionFitTwoPages: TAction;
    actionGotoFirstPage: TAction;
    actionGotoLastPage: TAction;
    actionGotoNextPage: TAction;
    actionGotoPreviousPage: TAction;
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
    actionCloseWindow: TFileExit;
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
    dxBarButton20: TdxBarButton;
    dxBarSeparator10: TdxBarSeparator;
    actionGotoInitialText: TAction;
    dxBarButton21: TdxBarButton;
    dxBarSeparator11: TdxBarSeparator;
    dxBarButton22: TdxBarButton;
    dxBarSeparator12: TdxBarSeparator;
    actionVersion: TAction;
    actionSendToBackground: TAction;
    actionSendToForeground: TAction;
    dxBarButton23: TdxBarButton;
    dxBarButton24: TdxBarButton;
    customActionList: TActionList;
    dxBarSubItem10: TdxBarSubItem;
    actionShowLog: TAction;
    dxBarButtonShowLog: TdxBarButton;
    dxDockPanel1: TdxDockPanel;
    cxMemo1: TcxMemo;
    dxDockSite2: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    procedure actionFitPageCustomExecute(Sender: TObject);
    procedure actionFitPageHeightExecute(Sender: TObject);
    procedure actionFitPageWidthExecute(Sender: TObject);
    procedure actionGotoFirstPageExecute(Sender: TObject);
    procedure actionGotoInitialTextExecute(Sender: TObject);
    procedure actionGotoLastPageExecute(Sender: TObject);
    procedure actionGotoNextPageExecute(Sender: TObject);
    procedure actionGotoPreviousPageExecute(Sender: TObject);
    procedure actionHideExecute(Sender: TObject);
    procedure actionPrintDialogExecute(Sender: TObject);
    procedure actionPrintExecute(Sender: TObject);
    procedure actionSendToBackgroundExecute(Sender: TObject);
    procedure actionSendToForegroundExecute(Sender: TObject);
    procedure actionShowLogExecute(Sender: TObject);
    procedure actionToggleFindPanelExecute(Sender: TObject);
    procedure actionVersionExecute(Sender: TObject);
    procedure actionZoomDecreaseExecute(Sender: TObject);
    procedure actionZoomIncreaseExecute(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure cxEditGotoCustomPageChange(Sender: TObject);
    procedure cxEditGotoCustomPagePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure dxBarButtonExitClick(Sender: TObject);
    procedure dxDockPanel1VisibleChanged(Sender: TdxCustomDockControl);
    procedure dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
    procedure Timer1Timer(Sender: TObject);
  private
    fLogObserver: TOPPHelpPreviewFormLogObserver;
    fSettings: TOPPHelpPreviewSettings;
    fDefaultPredicate: TOPPHelpPredicate;
    fCurrentState: TOPPHelpPreviewFormState;
    fNavigator: IOPPNavigator;
    oppHelpView: TOPPHelpViewFullScreen;
    reloadIsInProgress: Boolean;
    fOPPTimer: TOPPThreadTimer;
    fIsProcessingOPPPredicateMessage: Boolean;

    function GetPageIndexPanel: TdxStatusBarPanel;
    function GetStatusPanel: TdxStatusBarPanel;
    function GetProgressPanel: TdxStatusBarPanel;
    { --- }

    procedure ParsePredicate(const AStream: TReadOnlyMemoryStream);
    procedure ReloadNavigationPanel(ANavigator: IOPPNavigator);
    procedure RestoreFromBackground();
    procedure hideProgressPanel;
    procedure showProgressPanel;

    procedure ProgressiveEventsCountChanged(AValue: Integer; AEventName: String);
    procedure ProgressStop;
    procedure ProgressStart(AEventName: String);
    { --- }

    function NavigationInfo(ANavigator: IOPPNavigator): TOPPHelpPreviewNavigationPanelInfo;
    procedure applyHints();
    procedure SetCurrentState(ACurrentState: TOPPHelpPreviewFormState);
    procedure SetIsProcessingOPPPredicateMessage(const Value: Boolean);
    class function GetApplicationTitle: String; static;
    function GetLogIsVisible: Boolean;
    property currentState: TOPPHelpPreviewFormState read fCurrentState write SetCurrentState;
    property InfoPanel: TdxStatusBarPanel read GetPageIndexPanel;
    property Navigator: IOPPNavigator read fNavigator;
    property IsProcessingOPPPredicateMessage: Boolean read fIsProcessingOPPPredicateMessage write SetIsProcessingOPPPredicateMessage;
    procedure ApplySettings(ASettings: TOPPHelpPreviewSettings);
    property LogIsVisible: Boolean read GetLogIsVisible;
  protected
    procedure OnMessageWMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure OnMessageWMOPPPredicate(var Msg: TMessage); message WM_OPPPredicate;
    procedure OnMessageWMUserZoom(var Msg: TMessage); message WM_OPPZoom;
    procedure OnMessageWMUserZoomFit(var Msg: TMessage); message WM_OPPZoomFit;
    procedure OnMessageWMScrollingType(var Msg: TMessage); message WM_OPPScrollingType;
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
  public
    { Public declarations }
    function GetContainerClassName: String;
    procedure PresentModal();
    function RunPredicate(const APredicate: TOPPHelpPredicate; ACompletion: TOPPHelpPreviewFormCompletion): TOPPHelpShortcutViewerExecutionResult;
    class property ApplicationTitle: String read GetApplicationTitle;
  end;

var
  OPPHelpPreviewForm: TOPPHelpPreviewForm;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  dxCustomPreview,
  OPP.Help.Shortcut.Server,
  OPP.Help.View.ZoomSettings,
  OPP.Help.Log, OPP.Help.System.Error,
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Types,
  OPP.Help.System.Application,
  OPP.Help.System.Files,
  OPP.Help.Map,
  OPP.Help.QuickJumpMenuBuilder,
  AsyncCalls;

const
  kContext = 'TOPPHelpPreviewForm';
  kZoomTwoPagesFactor = 81;
  kZoomDefaultIncrement = 5;

resourcestring
  SEventReceivedMessageWM_COPYDATA = 'Received message: WM_COPYDATA';
  SErrorPredicateIsNotDefined = 'Predicate is not defined';
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
  SStatusFormCreated = 'Документ не загружен'; // 'Form created'
  SDefaultCaption = 'ГОЛЬФСТРИМ Помощь';
  SActionCloseWindowHint = 'Спрятать окно';
  SActionFitPageCustomHint = 'Подобрать размер';
  SActionFitPageHeightHint = 'Подобрать размер по высоте';
  SActionFitPageWidthHint = 'Подобрать размер по ширине';
  SActionFitTwoPagesHint = 'Подобрать размер для двух страниц';
  SActionGotoFirstPageHint = 'Перейти на первую страницу';
  SActionGotoLastPageHint = 'Перейти на последнюю страницу';
  SActionGotoNextPageHint = 'Перейти на следующую страницу';
  SActionGotoPreviousPageHint = 'Перейти на предыдущую страницу';
  SActionHideHint = 'Спрятать';
  SActionPrintHint = 'Распечатать';
  SActionPrintDialogHint = 'Распечатать, используя диалог';
  SActionToggleFindPanelHint = 'Найти текст';
  SActionZoomDecreaseHint = 'Уменьшить масштаб';
  SActionZoomIncreaseHint = 'Увеличить масштаб';
  SActionGotoInitialTextHint = 'Изначальная страница';

procedure TOPPHelpPreviewForm.actionFitPageCustomExecute(Sender: TObject);
var
  specialZoom: TOPPHelpPreviewZoomForm;
begin
  specialZoom := TOPPHelpPreviewZoomForm.Create(self);
  try
    specialZoom.Settings := fSettings;
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

procedure TOPPHelpPreviewForm.actionGotoFirstPageExecute(Sender: TObject);
begin
  if Assigned(Navigator) then
    Navigator.GotoFirstPage;
end;

procedure TOPPHelpPreviewForm.actionGotoInitialTextExecute(Sender: TObject);
begin
  RunPredicate(fDefaultPredicate,
    procedure()
    begin
    end);
end;

procedure TOPPHelpPreviewForm.actionGotoLastPageExecute(Sender: TObject);
begin
  if Assigned(Navigator) then
    Navigator.GotoLastPage;
end;

procedure TOPPHelpPreviewForm.actionGotoNextPageExecute(Sender: TObject);
begin
  if Assigned(Navigator) then
    Navigator.GotoNextPage;
end;

procedure TOPPHelpPreviewForm.actionGotoPreviousPageExecute(Sender: TObject);
begin
  if Assigned(Navigator) then
    Navigator.GotoPreviousPage;
end;

procedure TOPPHelpPreviewForm.actionHideExecute(Sender: TObject);
begin
  Close;
end;

procedure TOPPHelpPreviewForm.actionPrintDialogExecute(Sender: TObject);
begin
  oppHelpView.ShowPrintDialog(dxComponentPrinter1);
end;

procedure TOPPHelpPreviewForm.actionPrintExecute(Sender: TObject);
begin
  oppHelpView.PrintCurrentPage(dxComponentPrinter1);
end;

procedure TOPPHelpPreviewForm.actionSendToBackgroundExecute(Sender: TObject);
begin
  self.Hide();
  WindowState := wsMinimized;
  TrayIcon1.Visible := true;
end;

procedure TOPPHelpPreviewForm.actionSendToForegroundExecute(Sender: TObject);
begin
  TrayIcon1.Visible := false;
  self.show();
  self.WindowState := TWindowState.wsNormal;
  Application.BringToFront();
end;

procedure TOPPHelpPreviewForm.actionShowLogExecute(Sender: TObject);
begin
  dxDockSite2.Visible := not LogIsVisible;
end;

procedure TOPPHelpPreviewForm.actionToggleFindPanelExecute(Sender: TObject);
begin
  oppHelpView.IsFindPanelVisible := not oppHelpView.IsFindPanelVisible;
end;

procedure TOPPHelpPreviewForm.actionVersionExecute(Sender: TObject);
begin
  Application.openMarketingWebPage;
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
  actionSendToBackground.Execute;
end;

procedure TOPPHelpPreviewForm.ApplicationEvents1Restore(Sender: TObject);
begin
  TrayIcon1.Visible := false;
end;

procedure TOPPHelpPreviewForm.applyHints;
begin
  actionCloseWindow.Hint := SActionCloseWindowHint;
  actionFitPageCustom.Hint := SActionFitPageCustomHint;
  actionFitPageHeight.Hint := SActionFitPageHeightHint;
  actionFitPageWidth.Hint := SActionFitPageWidthHint;
  actionFitTwoPages.Hint := SActionFitTwoPagesHint;
  actionGotoFirstPage.Hint := SActionGotoFirstPageHint;
  actionGotoLastPage.Hint := SActionGotoLastPageHint;
  actionGotoNextPage.Hint := SActionGotoNextPageHint;
  actionGotoPreviousPage.Hint := SActionGotoPreviousPageHint;
  actionHide.Hint := SActionHideHint;
  actionPrint.Hint := SActionPrintHint;
  actionPrintDialog.Hint := SActionPrintDialogHint;
  actionToggleFindPanel.Hint := SActionToggleFindPanelHint;
  actionZoomDecrease.Hint := SActionZoomDecreaseHint;
  actionZoomIncrease.Hint := SActionZoomIncreaseHint;
  actionGotoInitialText.Hint := SActionGotoInitialTextHint;
end;

procedure TOPPHelpPreviewForm.ApplySettings(ASettings: TOPPHelpPreviewSettings);
begin
  if not Assigned(ASettings) then
    exit;

  oppHelpView.ScrollingType := ASettings.ScrollingType;

  case ASettings.ZoomMode of
    zmFitHeight:
      oppHelpView.FitPageHeight;
    zmFitWidth:
      oppHelpView.FitPageWidth;
    zmTwoColumns:
      oppHelpView.FitTwoColumns;
    zmCustom:
      oppHelpView.FitCustom(ASettings.ZoomScale);
  end;
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

procedure TOPPHelpPreviewForm.dxDockPanel1VisibleChanged(Sender: TdxCustomDockControl);
begin
  if dxDockSite2.Visible then
    actionShowLog.Caption := 'Спрятать лог'
  else
    actionShowLog.Caption := 'Показать лог';
end;

procedure TOPPHelpPreviewForm.dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := false;
end;

procedure TOPPHelpPreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  if Assigned(fOPPTimer) then
  begin
    fOPPTimer.FinishThreadExecution;
    fOPPTimer := nil;
  end;

  self.SaveFormState;

  oppHelpView.removeStateChangeListener(self);

  if Assigned(fDefaultPredicate) then
  begin
    FreeAndNil(fDefaultPredicate);
  end;
  eventLogger.UnregisterObserver(fLogObserver);
end;

procedure TOPPHelpPreviewForm.FormCreate(Sender: TObject);
begin
  fLogObserver := TOPPHelpPreviewFormLogObserver.Create;
  fLogObserver.cxMemo := self.cxMemo1;
  eventLogger.RegisterObserver(fLogObserver);
  dxDockSite2.Visible := false;
{$IF DEFINED (DEBUG)}
  dxBarButtonShowLog.Visible := ivAlways;
{$ELSE}
  dxBarButtonShowLog.Visible := ivNever;
{$ENDIF}
  IsProcessingOPPPredicateMessage := false;

  fSettings := TOPPHelpPreviewSettings.LoadOrCreate;

  actionVersion.Caption := Application.BuildNumber;

  self.ReadFormState;

  TOPPHelpQuickJumpMenuBuilder.Build(self,
    procedure(AAction: TAction)
    var
      fLink: TdxBarItemLink;
    begin
      AAction.ActionList := customActionList;
      with buttonQuickNavigation.ItemLinks.AddButton do
        Item.Action := AAction;
      with dxBarSubItem10.ItemLinks.AddButton do
        Item.Action := AAction;
    end,
    procedure(AHelpMap: TOPPHelpMap)
    begin
      if Assigned(AHelpMap) then
      begin
        oppHelpView.DoSearchIfPossible(AHelpMap.Predicate, siPredicate,
          procedure()
          begin
            //
          end);
      end;
    end);

  reloadIsInProgress := false;
  self.currentState := fsFormCreated;

  oppHelpView := TOPPHelpViewFullScreen.Create(self);
  oppHelpView.Parent := self;
  oppHelpView.Align := alClient;
  oppHelpView.OnStatusChanged := self.OnViewStatusChanged;
  self.OnViewStatusChanged(oppHelpView.Status);

  oppHelpView.LoadDefaultResource('');

  oppHelpView.OnFindPanelVisibilityChange := procedure(AValue: Boolean)
    begin
      findPaneTogglerButton.Down := AValue;
    end;

  fNavigator := oppHelpView as IOPPNavigator;
  self.Navigator.SetNavigatorStatusChangesCompletion(
    procedure(ANavigator: IOPPNavigator)
    begin
      if Assigned(ANavigator) then
      begin
        self.ReloadNavigationPanel(ANavigator);
      end;
    end);

  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);
  ReloadNavigationPanel(nil);
  applyHints();

  TOPPHelpViewCommandLine.ReadFromCommandLine(
    procedure(const AValue: TOPPHelpPredicate)
    begin

      if not Assigned(AValue) then
      begin
        eventLogger.Flow('Parsed command-line: no predicate', kContext);
        exit;
      end;
      if not AValue.isRunnable then
      begin
        eventLogger.Flow('Parsed command-line: predicate is not runnable', kContext);
        exit;
      end;

      RunPredicate(AValue,
        procedure()
        begin
          eventLogger.Flow('Parsed command-line: finished predicate execution', kContext);
        end);
    end);
end;

procedure TOPPHelpPreviewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(fNavigator) then
  begin
    fNavigator.SetNavigatorStatusChangesCompletion(nil);
  end;
end;

class function TOPPHelpPreviewForm.GetApplicationTitle: String;
begin
  result := SDefaultCaption;
end;

function TOPPHelpPreviewForm.GetContainerClassName: String;
begin
  result := self.className;
end;

function TOPPHelpPreviewForm.GetLogIsVisible: Boolean;
begin
  result := dxDockSite2.Visible;
end;

function TOPPHelpPreviewForm.GetPageIndexPanel: TdxStatusBarPanel;
begin
  result := dxStatusBar1.Panels[0];
end;

function TOPPHelpPreviewForm.GetProgressPanel: TdxStatusBarPanel;
begin
  result := dxStatusBar1.Panels[2];
end;

function TOPPHelpPreviewForm.GetStatusPanel: TdxStatusBarPanel;
begin
  result := dxStatusBar1.Panels[1];
end;

procedure TOPPHelpPreviewForm.hideProgressPanel;
begin
  cxProgressBar1.Position := 0;
  cxProgressBar1.Visible := false;
end;

{ --------- }

procedure TOPPHelpPreviewForm.OnMessageWMCopyData(var Msg: TWMCopyData);
var
  fNotificationStream: TReadOnlyMemoryStream;
begin
  eventLogger.Flow('Received WM_CopyData message', kContext);

  actionSendToForeground.Execute;

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  try
    ParsePredicate(fNotificationStream);
  finally
    fNotificationStream.Free;
  end;

  SendMessage(self.Handle, WM_OPPPredicate, 0, 0);
  eventLogger.Flow('Sent WM_OPPPredicate message', kContext);

  Msg.result := NativeInt(TOPPMessagePipeSendResult.psrSuccess);

end;

procedure TOPPHelpPreviewForm.OnMessageWMOPPPredicate(var Msg: TMessage);
var
  fResult: TOPPHelpShortcutViewerExecutionResult;
begin

  eventLogger.Flow('Received WM_OPPPredicate message', kContext);

  if IsProcessingOPPPredicateMessage then
  begin
    Msg.result := NativeInt(TOPPHelpShortcutViewerExecutionResult.erFailed);
    exit;
  end;

  oppHelpView.LockUpdates;
  IsProcessingOPPPredicateMessage := true;

  fResult := RunPredicate(fDefaultPredicate,
    procedure
    begin
      IsProcessingOPPPredicateMessage := false;
      TThread.Synchronize(nil,
        procedure
        begin
        end);
      ApplySettings(fSettings);
      oppHelpView.UnlockUpdates;
    end);

  Msg.result := NativeInt(fResult);
end;

procedure TOPPHelpPreviewForm.OnMessageWMScrollingType(var Msg: TMessage);
begin
  eventLogger.Flow('Received WM_ScrollingType message', kContext);
  if not Assigned(fSettings) then
    exit;

  fSettings.ScrollingType := TOPPHelpScrollingType(Msg.WParam);
  TOPPHelpPreviewSettings.Save(fSettings);

  oppHelpView.ScrollingType := fSettings.ScrollingType;
end;

procedure TOPPHelpPreviewForm.OnMessageWMUserZoom(var Msg: TMessage);
begin
  eventLogger.Flow('Received WM_UserZoom message', kContext);
end;

procedure TOPPHelpPreviewForm.OnMessageWMUserZoomFit(var Msg: TMessage);
var
  fFinalZoomFactor: Integer;
begin
  eventLogger.Flow('Received WM_UserZoomFit message', kContext);
  fFinalZoomFactor := Integer(Msg.LParam);

  if Assigned(fSettings) then
  begin
    fSettings.ZoomMode := TOPPHelpPreviewZoomMode(Msg.WParam);
    fSettings.ZoomScale := fFinalZoomFactor;

    ApplySettings(fSettings);
    fFinalZoomFactor := oppHelpView.ZoomFactor;
    fSettings.ZoomScale := fFinalZoomFactor;

    TOPPHelpPreviewSettings.Save(fSettings);
  end;

  Msg.result := fFinalZoomFactor;
end;

procedure TOPPHelpPreviewForm.OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
begin
  fitActualSizeButton.Down := (AStatus.ZoomMode = TdxPreviewZoomMode.pzmPageWidth);
  zoomValueEdit.EditValue := AStatus.ZoomFactor;
  zoomMenuButton.Caption := Format(SScalingTemplate, [AStatus.ZoomFactor]);
end;

procedure TOPPHelpPreviewForm.ParsePredicate(const AStream: TReadOnlyMemoryStream);
begin
  if Assigned(fDefaultPredicate) then
  begin
    FreeAndNil(fDefaultPredicate);
  end;

  fDefaultPredicate := TOPPHelpPredicate.Create();
  try
    fDefaultPredicate.readFromStream(AStream, true);
    eventLogger.Flow(Format(SEventParsedPredicateTemplate, [fDefaultPredicate.asString]), OPP.Help.View.Fullscreen.kContext);
  except
    on Error: Exception do
    begin
      eventLogger.Error(Error, kContext);
    end;
  end;
end;

procedure TOPPHelpPreviewForm.PresentModal;
begin
  ShowModal;
end;

procedure TOPPHelpPreviewForm.ProgressiveEventsCountChanged(AValue: Integer; AEventName: String);
begin
  Application.ProcessMessages;
  if AValue <= 0 then
  begin
    ProgressStop;
  end else begin
    ProgressStart(AEventName);
  end;
end;

procedure TOPPHelpPreviewForm.ProgressStart(AEventName: String);
var
  panel: TdxStatusBarPanel;
begin
  if Assigned(fOPPTimer) then
  begin
    fOPPTimer.Terminate;
    fOPPTimer := nil;
  end;

  fOPPTimer := TOPPThreadTimer.Create(true,
    procedure()
    begin
      self.Timer1Timer(nil);
    end);
  fOPPTimer.Start();

  self.showProgressPanel;

  panel := GetStatusPanel;
  if Assigned(panel) then
  begin
    panel.Text := AEventName;
    panel.Visible := true;
  end;

  panel := GetPageIndexPanel;
  if Assigned(panel) then
    panel.Visible := false;
  dxStatusBar1.Refresh;
end;

procedure TOPPHelpPreviewForm.ProgressStop;
var
  panel: TdxStatusBarPanel;
begin
  if Assigned(fOPPTimer) then
  begin
    fOPPTimer.Terminate;
    fOPPTimer := nil;
  end;

  self.hideProgressPanel;

  panel := GetStatusPanel;
  if Assigned(panel) then
  begin
    panel.Visible := false;
  end;

  panel := GetPageIndexPanel;
  if Assigned(panel) then
    panel.Visible := true;
  dxStatusBar1.Refresh;
end;

function TOPPHelpPreviewForm.NavigationInfo(ANavigator: IOPPNavigator): TOPPHelpPreviewNavigationPanelInfo;
begin
  if Assigned(ANavigator) then
  begin
    result.constraints := ANavigator.NavigatorConstraints;
    result.gotopage := (ANavigator.GetPageIndex + 1);
  end else begin
    result.constraints := [];
    result.gotopage := null;
  end;
end;

procedure TOPPHelpPreviewForm.ReloadNavigationPanel(ANavigator: IOPPNavigator);
var
  fNavigationInfo: TOPPHelpPreviewNavigationPanelInfo;
  i: Integer;
  panel: TdxStatusBarPanel;
begin
  reloadIsInProgress := true;

  fNavigationInfo := NavigationInfo(ANavigator);

  for i := 0 to customActionList.ActionCount - 1 do
  begin
    customActionList[i].Enabled := fNavigationInfo.hasConstraints;
  end;

  panel := GetPageIndexPanel;
  if Assigned(panel) and Assigned(ANavigator) then
  begin
    panel.Text := ANavigator.GetPageInfo;
  end;

  actionGotoInitialText.Enabled := fNavigationInfo.hasConstraints;
  actionZoomIncrease.Enabled := fNavigationInfo.hasConstraints;
  actionZoomDecrease.Enabled := fNavigationInfo.hasConstraints;
  actionFitTwoPages.Enabled := fNavigationInfo.hasConstraints;
  actionFitPageCustom.Enabled := fNavigationInfo.hasConstraints;
  actionFitPageWidth.Enabled := fNavigationInfo.hasConstraints;
  actionFitPageHeight.Enabled := fNavigationInfo.hasConstraints;
  cxEditGotoCustomPage.Enabled := fNavigationInfo.hasConstraints;
  dropdownPageSelectionEdit.Enabled := fNavigationInfo.hasConstraints;
  actionToggleFindPanel.Enabled := fNavigationInfo.hasConstraints;
  buttonQuickNavigation.Enabled := fNavigationInfo.hasConstraints;

  actionPrint.Enabled := fNavigationInfo.hasConstraints;
  actionPrintDialog.Enabled := fNavigationInfo.hasConstraints;

  actionGotoFirstPage.Enabled := ncCanGoFirstPage in fNavigationInfo.constraints;
  actionGotoPreviousPage.Enabled := ncCanGoPreviousPage in fNavigationInfo.constraints;
  actionGotoNextPage.Enabled := ncCanGoNextPage in fNavigationInfo.constraints;
  actionGotoLastPage.Enabled := ncCanGoLastPage in fNavigationInfo.constraints;

  cxEditGotoCustomPage.Enabled := fNavigationInfo.hasConstraints;

  cxEditGotoCustomPage.Properties.LockUpdate(true);
  cxEditGotoCustomPage.EditValue := fNavigationInfo.gotopage;
  cxEditGotoCustomPage.Properties.LockUpdate(false);
  reloadIsInProgress := false;
end;

procedure TOPPHelpPreviewForm.RestoreFromBackground;
begin
end;

function TOPPHelpPreviewForm.RunPredicate(const APredicate: TOPPHelpPredicate; ACompletion: TOPPHelpPreviewFormCompletion): TOPPHelpShortcutViewerExecutionResult;
begin
  result := TOPPHelpShortcutViewerExecutionResult.erSuccess;
  if not Assigned(APredicate) then
  begin
    result := TOPPHelpShortcutViewerExecutionResult.erFailed;
    eventLogger.Error(SErrorPredicateIsNotDefined, kContext);
    if Assigned(ACompletion) then
      ACompletion();
    exit;
  end;

  eventLogger.Flow(Format(SEventStartedPDFLoadTemplate, [APredicate.filename]), OPP.Help.View.Fullscreen.kContext);
  helpShortcutServer.loadPDF(TOPPHelpSystemFilesHelper.AbsolutePath(APredicate.filename),
    procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus)
    begin
      eventLogger.Flow(Format(SEventFinishedPDFLoadTemplate, [APredicate.filename]), OPP.Help.View.Fullscreen.kContext);
      if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssError then
      begin
        self.Caption := Format('%s', [TOPPHelpPreviewForm.ApplicationTitle]);
        if Assigned(ACompletion) then
          ACompletion();
        exit;
      end;

      self.Caption := Format('%s: %s', [TOPPHelpPreviewForm.ApplicationTitle, TPath.GetFileNameWithoutExtension(APredicate.filename)]);

      try
        case AStatus of
          TOPPHelpShortcutServerLoadStreamStatus.ssCreated:
            begin
              oppHelpView.LoadContent(AStream,
                procedure()
                begin
                  oppHelpView.DoSearchIfPossible(APredicate, siPredicate, ACompletion);
                end);
            end;
          TOPPHelpShortcutServerLoadStreamStatus.ssReused:
            begin
              oppHelpView.DoSearchIfPossible(APredicate, siPredicate, ACompletion);
            end
        else
          begin
            if Assigned(ACompletion) then
              ACompletion();
          end;
        end;

      except
        on E: Exception do
        begin
          eventLogger.Error(E, kContext);
          if Assigned(ACompletion) then
            ACompletion();
        end;
      end;
    end);
end;

procedure TOPPHelpPreviewForm.SetCurrentState(ACurrentState: TOPPHelpPreviewFormState);
var
  fStateStr: String;
begin
  fCurrentState := ACurrentState;

  fStateStr := fCurrentState.asString;
  InfoPanel.Text := fStateStr;
  eventLogger.Flow(Format(SEventStateTemplate, [fStateStr]), kContext);
end;

procedure TOPPHelpPreviewForm.SetIsProcessingOPPPredicateMessage(const Value: Boolean);
begin
  fIsProcessingOPPPredicateMessage := Value;
  actionHide.Enabled := not fIsProcessingOPPPredicateMessage;
end;

procedure TOPPHelpPreviewForm.showProgressPanel;
begin
  cxProgressBar1.Position := 0;
  cxProgressBar1.Visible := true;
end;

procedure TOPPHelpPreviewForm.Timer1Timer(Sender: TObject);
begin
  if cxProgressBar1.Position < cxProgressBar1.Properties.Max then
    cxProgressBar1.Position := cxProgressBar1.Position + 1
  else
    cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.TrayIcon1Click(Sender: TObject);
begin
  actionSendToForeground.Execute;
end;

procedure TOPPHelpPreviewForm.WMEnterSizeMove(var Message: TMessage);
begin
  //
  oppHelpView.Perform(WM_SETREDRAW, 0, 0);
end;

procedure TOPPHelpPreviewForm.WMExitSizeMove(var Message: TMessage);
begin
  //
  oppHelpView.Perform(WM_SETREDRAW, 1, 0);
  RedrawWindow(self.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

{ TOPPHelpPreviewFormStateHelper }

function TOPPHelpPreviewFormStateHelper.asString: String;
begin
  result := '';
  case self of
    fsFormCreated:
      result := SStatusFormCreated;
    fsLoadContentStarted:
      result := SStatusLoadContentStarted;
    fsLoadContentFinished:
      result := SStatusLoadContentFinished;
    fsHandlingMessage:
      result := SStatusHandling;
    fsSearchStarted:
      result := SStatusSearchStarted;
    fsSearchProgressing:
      result := SStatusSearchProgressing;
    fsSearchFinished:
      result := SStatusSearchFinished;
    fsIdle:
      result := SStatusIdle;
  end;
end;

{ TOPPHelpPreviewNavigationPanelInfoHelper }

function TOPPHelpPreviewNavigationPanelInfoHelper.hasConstraints: Boolean;
begin
  result := (self.constraints <> []);
end;

end.
