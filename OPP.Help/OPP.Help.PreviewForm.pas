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

  System.Classes, System.SysUtils, System.Variants,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.AppEvnts,
  Winapi.Messages, Winapi.Windows,
  Vcl.ActnList, System.Actions, Vcl.StdActns, dxDBSparkline, dxNumericWheelPicker,
  dxSkinsCore, dxSkinBasic, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkroom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin,
  dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue,
  dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White,
  dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinOffice2019Black, dxSkinOffice2019Colorful,
  dxSkinOffice2019DarkGray, dxSkinOffice2019White, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp,
  dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinTheBezier, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TOPPHelpPreviewFormState = (fsCreated, fsLoading, fsHandlingMessage, fsSearching, fsSearchProgressing, fsSearchFinishing, fsIdle);

  TOPPHelpPreviewFormStateHelper = record helper for TOPPHelpPreviewFormState
  public
    function asString(): String;
  end;

  TOPPHelpPreviewForm = class(TForm, IOPPHelpViewEventListener, IOPPHelpShortcutViewer)
    actionFitPageHeight: TAction;
    actionFitPageWidth: TAction;
    actionHide: TAction;
    ActionList1: TActionList;
    actionToggleFindPanel: TAction;
    ApplicationEvents1: TApplicationEvents;
    cxBarEditItem1: TcxBarEditItem;
    cxBarEditItem2: TcxBarEditItem;
    cxBarEditItem3: TcxBarEditItem;
    cxBarEditItem4: TcxBarEditItem;
    cxBarEditItem5: TcxBarEditItem;
    cxProgressBar1: TcxProgressBar;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButtonExit: TdxBarButton;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarLargeButton4: TdxBarLargeButton;
    dxBarLargeButton5: TdxBarLargeButton;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar2: TdxBar;
    dxBarSeparator1: TdxBarSeparator;
    dxBarSeparator2: TdxBarSeparator;
    dxBarSeparator3: TdxBarSeparator;
    dxBarSeparator4: TdxBarSeparator;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarSubItem4: TdxBarSubItem;
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
    procedure actionFitPageHeightExecute(Sender: TObject);
    procedure actionFitPageWidthExecute(Sender: TObject);
    procedure actionHideExecute(Sender: TObject);
    procedure actionToggleFindPanelExecute(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure cxBarEditItem4Change(Sender: TObject);
    procedure dxBarButtonExitClick(Sender: TObject);
    procedure dxBarLargeButton6Click(Sender: TObject);
    procedure dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure zoomValueEditChange(Sender: TObject);
  private
    fCurrentState: TOPPHelpPreviewFormState;
    { Private declarations }

    oppHelpView: TOPPHelpViewFullScreen;
    function GetInfoPanel: TdxStatusBarPanel;
    { --- }
    procedure LoadStarted();
    procedure OnMessageWMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure OnMessageWMUserZoom(var Msg: TMessage); message WM_USER + 3;
    procedure OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
    procedure ProgressIncrement;
    procedure RestoreFromBackground();
    procedure SearchEnded();
    procedure SearchProgress();
    procedure SearchStarted();
    { --- }

    procedure SendToBackground();
    procedure setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
    property currentState: TOPPHelpPreviewFormState read fCurrentState write setCurrentState;
    property InfoPanel: TdxStatusBarPanel read GetInfoPanel;
  public
    { Public declarations }
    function GetContainerClassName: String;
    procedure presentModal();
    procedure runPredicate(APredicate: TOPPHelpPredicate);
  end;

var
  OPPHelpPreviewForm: TOPPHelpPreviewForm;

implementation

{$R *.dfm}

uses
  dxCustomPreview,
  OPP.Help.Shortcut.Server,
  OPP.Help.System.Stream,
  OPP.Help.Preview.Zoom,
  OPP.Help.Log;

resourcestring
  SFlowReceivedMessage = 'Received Message';
  SScalingTemplate = 'Масштаб %d %%';
  SStatusIdle = '';
  SStatusFinishing = 'Finishing';
  SStatusProgressing = 'Progressing';
  SStatusSearching = 'Searching';
  SStatusHandling = 'Handling';
  SStatusLoading = 'Loading';
  SStatusCreated = 'Created';

const
  SMessageResultSuccess = 10000;

procedure TOPPHelpPreviewForm.actionFitPageHeightExecute(Sender: TObject);
begin
  oppHelpView.FitPageHeight();
end;

procedure TOPPHelpPreviewForm.actionFitPageWidthExecute(Sender: TObject);
begin
  oppHelpView.FitPageWidth();
end;

procedure TOPPHelpPreviewForm.actionHideExecute(Sender: TObject);
begin
  Close;
  // Application.Minimize;
end;

procedure TOPPHelpPreviewForm.actionToggleFindPanelExecute(Sender: TObject);
begin
  oppHelpView.IsFindPanelVisible := not oppHelpView.IsFindPanelVisible;
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

procedure TOPPHelpPreviewForm.dxBarButtonExitClick(Sender: TObject);
begin
  self.Close();
end;

procedure TOPPHelpPreviewForm.dxBarLargeButton6Click(Sender: TObject);
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

  self.currentState := fsCreated;

  oppHelpView := TOPPHelpViewFullScreen.Create(self);
  oppHelpView.Parent := self;
  oppHelpView.Align := alClient;
  oppHelpView.OnStatusChanged := self.OnViewStatusChanged;
  self.OnViewStatusChanged(oppHelpView.Status);

  oppHelpView.OnFindPanelVisibilityChange := procedure(AValue: Boolean)
    begin
      findPaneTogglerButton.Down := AValue;
    end;

  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);
end;

function TOPPHelpPreviewForm.GetContainerClassName: String;
begin
  Result := self.className;
end;

function TOPPHelpPreviewForm.GetInfoPanel: TdxStatusBarPanel;
begin
  Result := dxStatusBar1.Panels[1];
end;

{ --------- }
procedure TOPPHelpPreviewForm.LoadStarted();
begin
  currentState := fsLoading;
end;

procedure TOPPHelpPreviewForm.OnMessageWMCopyData(var Msg: TWMCopyData);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
begin
  eventLogger.Flow(SFlowReceivedMessage, OPP.Help.View.Fullscreen.kEventFlowName);

  currentState := fsHandlingMessage;

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  try
    fPredicate := TOPPHelpPredicate.Create();
    try
      fPredicate.readFromStream(fNotificationStream, true);
      runPredicate(fPredicate);
    finally
      fPredicate.Free;
    end;
  finally
    fNotificationStream.Free;
  end;

  Msg.Result := SMessageResultSuccess;
end;

procedure TOPPHelpPreviewForm.OnMessageWMUserZoom(var Msg: TMessage);
begin
  oppHelpView.ZoomFactor := Msg.WParam;
end;

procedure TOPPHelpPreviewForm.OnViewStatusChanged(AStatus: TOPPHelpViewFullScreenStatus);
begin
  fitActualSizeButton.Down := (AStatus.zoomMode = TdxPreviewZoomMode.pzmPageWidth);
  zoomValueEdit.EditValue := AStatus.ZoomFactor;
  zoomMenuButton.Caption := Format(SScalingTemplate, [AStatus.ZoomFactor]);
end;

procedure TOPPHelpPreviewForm.presentModal;
begin
  ShowModal;
end;

procedure TOPPHelpPreviewForm.ProgressIncrement;
begin
  if cxProgressBar1.Position = cxProgressBar1.Properties.Max then
    cxProgressBar1.Position := cxProgressBar1.Properties.Min
  else
    cxProgressBar1.Position := 1 + cxProgressBar1.Position;
end;

procedure TOPPHelpPreviewForm.RestoreFromBackground;
begin
  Application.Restore;
  Application.BringToFront();
  self.show();
  self.WindowState := TWindowState.wsNormal;
end;

procedure TOPPHelpPreviewForm.runPredicate(APredicate: TOPPHelpPredicate);
begin
  helpShortcutServer.loadPDF(APredicate.fileName,
    procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus)
    begin
      if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssError then
        exit;

      eventLogger.Flow(Format('started running predicate: %s', [APredicate.value]), OPP.Help.View.Fullscreen.kEventFlowName);

      try
        eventLogger.Flow(Format('loaded pdf from: %s', [APredicate.fileName]), OPP.Help.View.Fullscreen.kEventFlowName);
        if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssCreated then
          oppHelpView.loadContent(AStream);
      finally
        oppHelpView.setPredicate(APredicate);
      end;
    end);
end;

procedure TOPPHelpPreviewForm.SearchEnded();
begin
  currentState := fsSearchFinishing;

  cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.SearchProgress();
begin
  currentState := fsSearchProgressing;

  if cxProgressBar1.Position = cxProgressBar1.Properties.Max then
    cxProgressBar1.Position := cxProgressBar1.Properties.Min
  else
    cxProgressBar1.Position := cxProgressBar1.Position + 1;
end;

procedure TOPPHelpPreviewForm.SearchStarted();
begin
  currentState := fsSearching;
  cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.SendToBackground;
begin
  self.Hide();
  self.WindowState := TWindowState.wsMinimized;

end;

procedure TOPPHelpPreviewForm.setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
var
  fStateStr: String;
begin
  fCurrentState := ACurrentState;

  fStateStr := ACurrentState.asString;

  InfoPanel.Text := fStateStr;

  eventLogger.Flow(Format('State: %s', [fStateStr]), 'PDFViewer');
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
  result := '';
  case self of
    fsCreated:
      result := SStatusCreated;
    fsLoading:
      result := SStatusLoading;
    fsHandlingMessage:
      result := SStatusHandling;
    fsSearching:
      result := SStatusSearching;
    fsSearchProgressing:
      result := SStatusProgressing;
    fsSearchFinishing:
      result := SStatusIdle;
    fsIdle:
      result := SStatusIdle;
  end;
end;

end.
