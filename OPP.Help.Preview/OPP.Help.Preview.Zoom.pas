unit OPP.Help.Preview.Zoom;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee, dxSkinDarkRoom,
  dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast,
  dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful,
  dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver,
  dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine,
  dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint,
  dxSkinXmas2008Blue, cxTextEdit, cxMaskEdit, cxSpinEdit, cxLabel, cxTrackBar, System.Actions, Vcl.ActnList,
  Vcl.StdActns, dxSkinBasic, dxSkinOffice2019Black, dxSkinOffice2019Colorful, dxSkinOffice2019DarkGray,
  dxSkinOffice2019White, dxSkinTheBezier;

type
  TOPPHelpPreviewZoomForm = class(TForm)
    cxLabel1: TcxLabel;
    cxSpinEdit1: TcxSpinEdit;
    cxTrackBar1: TcxTrackBar;
    cxLabel2: TcxLabel;
    ActionList1: TActionList;
    Action1: TAction;
    procedure cxTrackBar1PropertiesChange(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure cxSpinEdit1PropertiesEditValueChanged(Sender: TObject);
  private
    { Private declarations }
    fZoomValue: Integer;
    procedure setZoomValue(AValue: Integer);
    procedure postMessage(AValue: Integer);

  public
    { Public declarations }
    property zoomValue: Integer read fZoomValue write setZoomValue;
  end;

var
  OPPHelpPreviewZoomForm: TOPPHelpPreviewZoomForm;

implementation

{$R *.dfm}

uses
OPP.Help.System.Str,
OPP.Help.System.Messaging;

procedure TOPPHelpPreviewZoomForm.setZoomValue(AValue: Integer);
begin
  fZoomValue := AValue;
  cxSpinEdit1.Value := AValue;
  cxTrackBar1.Position := AValue;
end;

procedure TOPPHelpPreviewZoomForm.Action1Execute(Sender: TObject);
begin
  close;
end;

procedure TOPPHelpPreviewZoomForm.cxSpinEdit1PropertiesEditValueChanged(Sender: TObject);
begin
  cxSpinEdit1.SelectAll;
  cxTrackBar1.position := cxSpinEdit1.Value;
  postMessage(cxSpinEdit1.Value);
end;

procedure TOPPHelpPreviewZoomForm.cxTrackBar1PropertiesChange(Sender: TObject);
begin
  cxSpinEdit1.Value := cxTrackBar1.Position;
  postMessage(cxTrackBar1.Position);
end;

procedure TOPPHelpPreviewZoomForm.postMessage(AValue: Integer);
var
  fHandle: THandle;
const
  fClassName: String = 'TOPPHelpPreviewForm';
begin
  fZoomValue := AValue;
  fHandle := FindWindow(fClassName.toWideChar(), nil);
  SendMessage(fHandle, WM_OPPZoom, fZoomValue, 0);
end;

end.
