unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, dxCore, dxCoreClasses, dxRichEdit.NativeApi,
  dxRichEdit.Types, dxRichEdit.PlainText, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxGDIPlusAPI, dxGDIPlusClasses, dxRichEdit.Options,
  dxRichEdit.Control, dxRichEdit.Control.SpellChecker, dxRichEdit.Dialogs.EventArgs, dxRichEdit.Platform.Win.Control,
  dxRichEdit.Control.Core,
  dxRichEdit.Api.Paragraphs,
  dxRichEdit.Api.Hyperlinks,
  dxRichEdit.Api.NativeDocument, dxScreenTip, cxClasses, dxCustomHint, cxHint, cxLabel, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue,

  cxHintEditor,

  OPP.System,
  OPP.Hint,
  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  OPP.dxRichEdit, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdActns, System.Actions, Vcl.ActnList;

type
  TSampleForm = class(TForm)
    LinkToText: TCheckBox;
    Text1BookmarkFixed: TEdit;
    cxHintController: TcxHintStyleController;
    Button2: TButton;
    tipsRepo: TdxScreenTipRepository;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    hintServer: OPPRichEditHintServer;
    hints: TList<TOPPHint>;

    procedure addTip(Hint: TOPPHint);
  public
    { Public declarations }
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.dfm}
{$IFDEF DEBUG}
{$C +}
{$ENDIF}

procedure TSampleForm.FormCreate(Sender: TObject);
const
  filepath: String = 'gulfstream_manual_rtf.rtf';
var
  loadResult: TOPPHintServerLoadResultType;
  fHints: TList<TOPPHint>;
  fRTF: String;
  fStream, fcxStream: TStringStream;
  fHint: TOPPHint;
begin

  hintServer := OPPRichEditHintServer.create;
  loadResult := hintServer.loadFromFile(filepath);
  if loadResult.error = nil then begin
    fHints := hintServer.GetHints(self);
    for fHint in fHints do begin
      self.addTip(fHint);
    end;
  end;

  self.restyle();
end;

procedure TSampleForm.addTip(Hint: TOPPHint);
var
  fTip: TdxScreenTip;
  fTipLink: TdxScreenTipLink;
  fControl: TControl;
begin
  fControl := self.FindFirst(Hint.meta.propertyName, Hint.meta.hintIdentifier);
  if not assigned(fControl) then
    exit;

  fTip := tipsRepo.Items.Add;
  fTip.Header.PlainText := true;
  fTip.Header.Text := 'Заголовок';

  fTip.Description.PlainText := false;
  fTip.Description.Text := Hint.data.rtf;

  fTip.Footer.PlainText := true;
  fTip.Footer.Text := 'Подвал';

  fTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
  fTipLink.ScreenTip := fTip;
  fTipLink.Control := fControl;
end;

procedure TSampleForm.Button2Click(Sender: TObject);
begin
  ShowHintStyleEditor(cxHintController);
end;

initialization

finalization

end.
