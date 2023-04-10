unit OPP.Help.View.ZoomSettings;

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
  Vcl.StdActns, Vcl.Menus, cxButtons, System.ImageList, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TOPPHelpPreviewZoomForm = class(TForm)
    ActionList1: TActionList;
    actionClose: TAction;
    Panel1: TPanel;
    cxLabel1: TcxLabel;
    cxSpinEdit1: TcxSpinEdit;
    Bevel1: TBevel;
    Panel2: TPanel;
    Panel3: TPanel;
    Bevel2: TBevel;
    Button1: TButton;
    ImageList1: TImageList;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxButton3: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure cxSpinEdit1PropertiesEditValueChanged(Sender: TObject);
  private
    { Private declarations }
    fZoomValue: Integer;
    fIsHandlingMessage: Boolean;
    procedure setZoomValue(AValue: Integer);
    procedure PostZoomChangeMessage(AValue: Integer);

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

procedure TOPPHelpPreviewZoomForm.FormCreate(Sender: TObject);
begin
fIsHandlingMessage := false;
end;

procedure TOPPHelpPreviewZoomForm.setZoomValue(AValue: Integer);
begin
  if AValue = fZoomValue then
    exit;
  fZoomValue := AValue;
  if cxSpinEdit1.Value <> AValue then
    cxSpinEdit1.Value := AValue;

  PostZoomChangeMessage(fZoomValue);
end;

procedure TOPPHelpPreviewZoomForm.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPHelpPreviewZoomForm.cxButton2Click(Sender: TObject);
var
  fTag: Integer;
  fHandle: THandle;
  messageResult: NativeInt;
const
  fClassName: String = 'TOPPHelpPreviewForm';
begin
  fIsHandlingMessage := true;
  fTag := TcxButton(Sender).Tag;
  fHandle := FindWindow(fClassName.toWideChar(), nil);
  messageResult := SendMessage(fHandle, WM_OPPZoomFit, fTag, 0);
  cxSpinEdit1.Properties.BeginUpdate;
  cxSpinEdit1.Value := messageResult;
  cxSpinEdit1.Properties.EndUpdate(false);
  fIsHandlingMessage := false;
end;

procedure TOPPHelpPreviewZoomForm.cxSpinEdit1PropertiesEditValueChanged(Sender: TObject);
begin
  if fIsHandlingMessage then exit;
  zoomValue := cxSpinEdit1.Value;
  // cxSpinEdit1.SelectAll;
end;

procedure TOPPHelpPreviewZoomForm.PostZoomChangeMessage(AValue: Integer);
var
  fHandle: THandle;
const
  fClassName: String = 'TOPPHelpPreviewForm';
begin
  fZoomValue := AValue;
  fHandle := FindWindow(fClassName.toWideChar(), nil);
  PostMessage(fHandle, WM_OPPZoom, fZoomValue, 0);
end;

end.
