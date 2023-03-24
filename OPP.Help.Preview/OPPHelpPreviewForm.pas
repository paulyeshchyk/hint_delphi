unit OPPHelpPreviewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, OPP.Help.View.Fullscreen, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel,
  dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy,
  dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin,
  dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, dxStatusBar,
  cxContainer, cxEdit, cxProgressBar, OPP.Help.View;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TForm1 = class(TForm, IOPPHelpViewEventListener)
    oppHelpView: TOPPHelpViewFullScreen;
    dxStatusBar1: TdxStatusBar;
    dxStatusBar1Container0: TdxStatusBarContainerControl;
    cxProgressBar1: TcxProgressBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fProgress: TcxProgressBar;
    fProgressPanel: TPanel;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure SearchStarted();
    procedure SearchProgress();
    procedure SearchEnded();

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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oppHelpView.removeStateChangeListener(self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxProgressBar1.Properties.ShowText := false;
  oppHelpView.addStateChangeListener(self);
  dxStatusBar1.Panels[1].Text := 'Created';
end;

procedure TForm1.WMCopyData(var Msg: TWMCopyData);
var
  fPredicate: TOPPHelpPredicate;
  fNotificationStream: TReadOnlyMemoryStream;
  fPDFStream: TMemoryStream;
begin
  dxStatusBar1.Panels[1].Text := 'received message';

  fNotificationStream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.readFromStream(fNotificationStream);

  fPDFStream := helpShortcutServer.loadPDF(fPredicate.fileName);
  oppHelpView.loadContent(fPDFStream);
  oppHelpView.setPredicate(fPredicate);

  fPredicate.Free;

end;

{ --------- }
procedure TForm1.SearchStarted();
begin
  dxStatusBar1.Panels[1].Text := 'SearchStarted';
  cxProgressBar1.Position := 0;
end;

procedure TForm1.SearchProgress();
begin
  dxStatusBar1.Panels[1].Text := 'SearchProgress';
  cxProgressBar1.Position := cxProgressBar1.Position + 1;
  if (cxProgressBar1.Position >= 100) then
    cxProgressBar1.Position := 0;
end;

procedure TForm1.SearchEnded();
begin
  dxStatusBar1.Panels[1].Text := 'SearchEnded';
  cxProgressBar1.Position := 0;
end;

end.
