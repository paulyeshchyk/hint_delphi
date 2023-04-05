unit OPP.Help.PreviewForm;

interface

uses
  cxBarEditItem, cxClasses, cxContainer, cxControls, cxEdit, cxGraphics,
  cxLookAndFeelPainters, cxLookAndFeels, cxPC, cxProgressBar, cxStyles, dxBar,
  dxDockControl, dxDockPanel, dxStatusBar,

  OPP.Help.Interfaces, OPP.Help.Predicate,
  OPP.Help.View.Fullscreen,

  System.Classes, System.SysUtils, System.Variants,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.AppEvnts,
  Winapi.Messages, Winapi.Windows, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin,
  dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue,
  dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White,
  dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp,
  dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, cxButtonEdit, cxSpinEdit,
  Vcl.ActnList, System.Actions, Vcl.StdActns;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TOPPHelpPreviewFormState = (fsCreated, fsLoading, fsHandlingMessage, fsSearching, fsSearchProgressing, fsSearchFinishing, fsIdle);

  TOPPHelpPreviewForm = class(TForm, IOPPHelpViewEventListener, IOPPHelpShortcutViewer)
    dxStatusBar1: TdxStatusBar;
    dxStatusBar1Container0: TdxStatusBarContainerControl;
    cxProgressBar1: TcxProgressBar;
    dxDockingManager1: TdxDockingManager;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButtonExit: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    dxBarManager1Bar2: TdxBar;
    fitActualSizeButton: TdxBarButton;
    actionFitPageWidth: TAction;
    actionFitPageHeight: TAction;
    fitPageWidthButton: TdxBarButton;
    cxBarEditItem1: TcxBarEditItem;
    zoomValueEdit: TcxBarEditItem;
    procedure ApplicationEvents1Activate(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure dxBarButtonExitClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure actionFitPageWidthExecute(Sender: TObject);
    procedure zoomValueEditChange(Sender: TObject);
    procedure actionFitPageHeightExecute(Sender: TObject);
  private
    { Private declarations }

    oppHelpView: TOPPHelpViewFullScreen;
    fCurrentState: TOPPHelpPreviewFormState;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    { --- }
    procedure LoadStarted();
    procedure SearchStarted();
    procedure SearchProgress();
    procedure SearchEnded();
    { --- }

    procedure SendToBackground();
    procedure RestoreFromBackground();

    procedure setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
    property currentState: TOPPHelpPreviewFormState read fCurrentState write setCurrentState;

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
  OPP.Help.System.Stream;

procedure TOPPHelpPreviewForm.actionFitPageHeightExecute(Sender: TObject);
begin
  oppHelpView.FitPageHeight();
end;

procedure TOPPHelpPreviewForm.actionFitPageWidthExecute(Sender: TObject);
begin
  oppHelpView.FitPageWidth();
end;

procedure TOPPHelpPreviewForm.ApplicationEvents1Activate(Sender: TObject);
begin
  //
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

function TOPPHelpPreviewForm.GetContainerClassName: String;
begin
  Result := self.className;
end;

procedure TOPPHelpPreviewForm.presentModal;
begin
  ShowModal;
end;

procedure TOPPHelpPreviewForm.setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
begin
  fCurrentState := ACurrentState;
  case fCurrentState of
    fsCreated:
      dxStatusBar1.Panels[1].Text := 'Created';
    fsLoading:
      dxStatusBar1.Panels[1].Text := 'Loading';
    fsHandlingMessage:
      dxStatusBar1.Panels[1].Text := 'Handling';
    fsSearching:
      dxStatusBar1.Panels[1].Text := 'Searching';
    fsSearchProgressing:
      dxStatusBar1.Panels[1].Text := 'Progressing';
    fsSearchFinishing:
      dxStatusBar1.Panels[1].Text := 'Finishing';
    fsIdle:
      dxStatusBar1.Panels[1].Text := '';
  end;
end;

procedure TOPPHelpPreviewForm.dxBarButtonExitClick(Sender: TObject);
begin
  self.close();
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

  oppHelpView := TOPPHelpViewFullScreen.Create(self);
  oppHelpView.Parent := self;
  oppHelpView.Align := alClient;
  oppHelpView.OnStatusChanged := procedure(AStatus: TOPPHelpViewFullScreenStatus)
  begin
    fitActualSizeButton.Down := (AStatus.zoomMode = TdxPreviewZoomMode.pzmPageWidth);
    zoomValueEdit.EditValue := AStatus.zoomFactor;
  end;

  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);
end;

procedure TOPPHelpPreviewForm.WMCopyData(var Msg: TWMCopyData);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
begin
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

  Msg.Result := 10000;
end;

procedure TOPPHelpPreviewForm.zoomValueEditChange(Sender: TObject);
begin
  oppHelpView.zoomFactor := Integer(zoomValueEdit.EditValue);
end;

{ --------- }
procedure TOPPHelpPreviewForm.LoadStarted();
begin
  currentState := fsLoading;
end;

procedure TOPPHelpPreviewForm.SearchStarted();
begin
  currentState := fsSearching;
  cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.SearchProgress();
begin
  currentState := fsSearchProgressing;

  cxProgressBar1.Position := cxProgressBar1.Position + 1;
  if (cxProgressBar1.Position >= 100) then
    cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.SearchEnded();
begin
  fCurrentState := fsSearchFinishing;

  cxProgressBar1.Position := 0;
end;

procedure TOPPHelpPreviewForm.runPredicate(APredicate: TOPPHelpPredicate);
begin
  helpShortcutServer.loadPDF(APredicate.fileName,
    procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus)
    begin
      try
        if AStatus = TOPPHelpShortcutServerLoadStreamStatus.ssCreated then
          oppHelpView.loadContent(AStream);
      finally
        oppHelpView.setPredicate(APredicate);
      end;
    end);
end;

procedure TOPPHelpPreviewForm.TrayIcon1Click(Sender: TObject);
begin
  self.RestoreFromBackground();
end;

procedure TOPPHelpPreviewForm.SendToBackground;
begin
  self.Hide();
  self.WindowState := TWindowState.wsMinimized;

end;

procedure TOPPHelpPreviewForm.RestoreFromBackground;
begin
  Application.Restore;
  Application.BringToFront();
  self.Show();
  self.WindowState := TWindowState.wsNormal;
end;

end.
