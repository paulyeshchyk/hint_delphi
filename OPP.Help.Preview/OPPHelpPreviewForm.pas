unit OPPHelpPreviewForm;

interface

uses
  cxBarEditItem, cxClasses, cxContainer, cxControls, cxEdit, cxGraphics,
  cxLookAndFeelPainters, cxLookAndFeels, cxPC, cxProgressBar, cxStyles, dxBar,
  dxDockControl, dxDockPanel, dxStatusBar,
  System.Classes, System.SysUtils,
  System.Variants,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls,
  Winapi.Messages, Winapi.Windows,
  OPP.Help.View, OPP.Help.View.Fullscreen, dxSkinsCore, dxSkinBasic, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkroom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle,
  dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky,
  dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinOffice2019Black,
  dxSkinOffice2019Colorful, dxSkinOffice2019DarkGray, dxSkinOffice2019White, dxSkinPumpkin, dxSkinSeven,
  dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust, dxSkinSummer2008,
  dxSkinTheAsphaltWorld, dxSkinTheBezier, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TOPPHelpPreviewFormState = (fsCreated, fsLoading, fsHandlingMessage, fsSearching, fsSearchProgressing, fsSearchFinishing, fsIdle);

  TForm1 = class(TForm, IOPPHelpViewEventListener)
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
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  OPP.Help.System.Messaging,
  OPP.Help.System.Stream,
  OPP.Help.Predicate,
  OPP.Help.Events,

  OPP.Help.Shortcut.Server;

procedure TForm1.setIsTreeVisible(AValue: Boolean);
begin
  fIsTreeVisible := AValue;
  //
end;

procedure TForm1.setCurrentState(ACurrentState: TOPPHelpPreviewFormState);
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

procedure TForm1.dxBarButton1Click(Sender: TObject);
begin
  oppHelpView.triggerFindPanel;
end;

procedure TForm1.dxBarButtonExitClick(Sender: TObject);
begin
  self.close();
end;

procedure TForm1.dxBarButtonShowTreeClick(Sender: TObject);
begin
  isTreeVisible := not isTreeVisible;
end;

procedure TForm1.dxDockPanel2CloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := false;
  isTreeVisible := false;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oppHelpView.removeStateChangeListener(self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);

  { --- }

  isTreeVisible := true;
end;

procedure TForm1.WMCopyData(var Msg: TWMCopyData);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
  fPDFStream: TMemoryStream;
begin
  currentState := fsHandlingMessage;

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.readFromStream(fNotificationStream);

  fPDFStream := helpShortcutServer.loadPDF(fPredicate.fileName);
  oppHelpView.loadContent(fPDFStream);
  oppHelpView.setPredicate(fPredicate);
end;

{ --------- }
procedure TForm1.LoadStarted();
begin
  currentState := fsLoading;
end;

procedure TForm1.SearchStarted();
begin
  currentState := fsSearching;
  cxProgressBar1.Position := 0;
end;

procedure TForm1.SearchProgress();
begin
  currentState := fsSearchProgressing;

  cxProgressBar1.Position := cxProgressBar1.Position + 1;
  if (cxProgressBar1.Position >= 100) then
    cxProgressBar1.Position := 0;
end;

procedure TForm1.SearchEnded();
begin
  fCurrentState := fsSearchFinishing;

  cxProgressBar1.Position := 0;
end;

end.
