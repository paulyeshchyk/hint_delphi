unit OPP.Help.PreviewForm;

interface

uses
  cxBarEditItem, cxClasses, cxContainer, cxControls, cxEdit, cxGraphics,
  cxLookAndFeelPainters, cxLookAndFeels, cxPC, cxProgressBar, cxStyles, dxBar,
  dxDockControl, dxDockPanel, dxStatusBar,

  OPP.Help.View, OPP.Help.View.Fullscreen, OPP.Help.Predicate,

  System.Classes, System.SysUtils, System.Variants,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls,
  Winapi.Messages, Winapi.Windows, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue,
  dxSkinBasic, dxSkinOffice2019Black, dxSkinOffice2019Colorful, dxSkinOffice2019DarkGray, dxSkinOffice2019White,
  dxSkinTheBezier;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TOPPHelpPreviewFormState = (fsCreated, fsLoading, fsHandlingMessage, fsSearching, fsSearchProgressing, fsSearchFinishing, fsIdle);

  TOPPHelpPreviewForm = class(TForm, IOPPHelpViewEventListener)
    dxStatusBar1: TdxStatusBar;
    dxStatusBar1Container0: TdxStatusBarContainerControl;
    cxProgressBar1: TcxProgressBar;
    dxDockingManager1: TdxDockingManager;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    oppHelpView: TOPPHelpViewFullScreen;
    dxBarButtonExit: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxBarButton1: TdxBarButton;
    dxBarSeparator1: TdxBarSeparator;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure dxBarButtonShowTreeClick(Sender: TObject);
    procedure dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure dxBarButton1Click(Sender: TObject);
    procedure dxBarButtonExitClick(Sender: TObject);
  private
    { Private declarations }
    fIsTreeVisible: Boolean;
    fCurrentState: TOPPHelpPreviewFormState;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    { --- }
    procedure LoadStarted();
    procedure SearchStarted();
    procedure SearchProgress();
    procedure SearchEnded();
    { --- }

    procedure setIsTreeVisible(AValue: Boolean);

    property isTreeVisible: Boolean read fIsTreeVisible write setIsTreeVisible;
    procedure setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
    property currentState: TOPPHelpPreviewFormState read fCurrentState write setCurrentState;

  public
    { Public declarations }
    procedure runPredicate(APredicate: TOPPHelpPredicate);
  end;

  // var
  // OPPHelpPreviewForm: TOPPHelpPreviewForm;

implementation

{$R *.dfm}

uses

  OPP.Help.System.Stream,

  OPP.Help.Shortcut.Server;

procedure TOPPHelpPreviewForm.setIsTreeVisible(AValue: Boolean);
begin
  fIsTreeVisible := AValue;
  //
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

procedure TOPPHelpPreviewForm.dxBarButton1Click(Sender: TObject);
begin
  oppHelpView.triggerFindPanel;
end;

procedure TOPPHelpPreviewForm.dxBarButtonExitClick(Sender: TObject);
begin
  self.close();
end;

procedure TOPPHelpPreviewForm.dxBarButtonShowTreeClick(Sender: TObject);
begin
  isTreeVisible := not isTreeVisible;
end;

procedure TOPPHelpPreviewForm.dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := false;
  isTreeVisible := false;
end;

procedure TOPPHelpPreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oppHelpView.removeStateChangeListener(self);
end;

procedure TOPPHelpPreviewForm.FormCreate(Sender: TObject);
begin
  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);

  isTreeVisible := true;
end;

procedure TOPPHelpPreviewForm.WMCopyData(var Msg: TWMCopyData);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
begin
  currentState := fsHandlingMessage;

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.readFromStream(fNotificationStream);

  runPredicate(fPredicate);
  Msg.Result := 10000;
  // fPredicate.Free;
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
var
  fPDFStream: TMemoryStream;
begin
  fPDFStream := helpShortcutServer.loadPDF(APredicate.fileName);
  try
    oppHelpView.loadContent(fPDFStream);
    oppHelpView.setPredicate(APredicate);
  finally
  end;
end;

end.
