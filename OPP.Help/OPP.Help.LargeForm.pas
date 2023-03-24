unit OPP.Help.LargeForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel,
  dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle,
  dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast,
  dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky,
  dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins,
  dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray,
  dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful,
  dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters,
  dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint,
  dxSkinXmas2008Blue, dxBar, cxClasses, cxPC, dxDockControl, dxDockPanel,
  cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxPDFDocument,
  dxBarBuiltInMenu, dxCustomPreview, dxPDFViewer, Vcl.ComCtrls, Vcl.ExtCtrls,

  OPP.Help.View,
  OPP.Help.View.Fullscreen,
  OPP.Help.Predicate,
  OPP.Help.Shortcut.Mapping, OPP.Help.System.Thread;

type
  TOPPHelpLargeForm = class(TForm, IOPPHelpViewEventListener)
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton1: TdxBarButton;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fHelpFullScreen: TOPPHelpViewFullScreen;
    fStream: TMemoryStream;
    fPredicate: TOPPHelpPredicate;
    fProgress: Integer;
    procedure searchStarted;
    procedure searchEnded;
    procedure searchProgress;
    procedure setStream(AStream: TMemoryStream);
    procedure setPredicate(APredicate: TOPPHelpPredicate);
    function doIncrementPosition(): Integer;
  public
    { Public declarations }
    property stream: TMemoryStream read fStream write setStream;
    property predicate: TOPPHelpPredicate read fPredicate write setPredicate;
  end;

var
  OPPHelpLargeForm: TOPPHelpLargeForm;

implementation

{$R *.dfm}

procedure TOPPHelpLargeForm.FormCreate(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  fProgress := 0;

  fHelpFullScreen := TOPPHelpViewFullScreen.Create(self);
  fHelpFullScreen.Align := alClient;
  fHelpFullScreen.Parent := self;
  fHelpFullScreen.addStateChangeListener(self);
end;

procedure TOPPHelpLargeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(fHelpFullScreen) then begin
    fHelpFullScreen.removeStateChangeListener(self);
    FreeAndNil(fHelpFullScreen);
  end;
end;

procedure TOPPHelpLargeForm.searchStarted();
begin
  ProgressBar1.Position := 0;
end;

procedure TOPPHelpLargeForm.searchEnded();
begin
  ProgressBar1.Position := 0;
end;

procedure TOPPHelpLargeForm.searchProgress;
begin
  ProgressBar1.Position := doIncrementPosition;
end;

function TOPPHelpLargeForm.doIncrementPosition: Integer;
begin
  fProgress := fProgress + 1;
  if fProgress >= 100 then
    fProgress := 0;
  result := fProgress;
end;

procedure TOPPHelpLargeForm.setPredicate(APredicate: TOPPHelpPredicate);
begin
  fHelpFullScreen.setPredicate(APredicate);
end;

procedure TOPPHelpLargeForm.setStream(AStream: TMemoryStream);
begin
  fHelpFullScreen.loadContent(AStream);
end;

end.
