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
  dxRichEdit.Api.NativeDocument, dxScreenTip, cxClasses, dxCustomHint, cxHint, cxLabel, Vcl.ExtCtrls,

  OPP.System,
  OPP.Hint,
  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  OPP.dxRichEdit, Vcl.ComCtrls, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue;

type
  TSampleForm = class(TForm)
    Button1: TButton;
    LinkToText: TCheckBox;
    Edit1: TEdit;
    cxRichEdit1: TcxRichEdit;
    cxHintStyleController1: TcxHintStyleController;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cxHintStyleController1ShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  private
    { Private declarations }

    hintServer: OPPRichEditHintServer;
    hints: TList<TOPPHint>;

    function getTextForBookmark(bookmarkName: String; document: IdxRichEditDocument): String;
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
begin
  hintServer := OPPRichEditHintServer.create;
  loadResult := hintServer.loadFromFile(filepath);
  if loadResult.error = nil then begin
    fHints := hintServer.GetHints(self);
    fRTF := fHints.First.data.rtf;
    fStream := TStringStream.create(fRTF);
    fcxStream := TStringStream.create(fRTF);
    self.cxRichEdit1.Lines.LoadFromStream(fcxStream);
  end;

  self.restyle;
end;

procedure TSampleForm.Button1Click(Sender: TObject);
var
  fHint: TOPPHint;
  fHintIdentifiers: TList<TOPPHintMeta>;
  hintText: String;
  fHintMeta: TOPPHintMeta;
begin

  fHintMeta.propertyName := '';
  fHintMeta.hintIdentifier := 'Text1BookmarkFixed';
  fHint := hintServer.GetHint(fHintMeta);

  fHintIdentifiers := self.GetControlHintsMeta();
  self.hints := hintServer.GetHints(fHintIdentifiers);

  hintText := fHint.data.text;

  OutputDebugString(hintText.toWideChar);
end;

procedure TSampleForm.cxHintStyleController1ShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  AFullText: String;
begin
  AFullText := HintInfo.HintControl.Hint;
  if Pos('|', AFullText) > 0 then begin
    Caption := Copy(AFullText, 0, Pos('|', AFullText) - 1);
    HintStr := Copy(AFullText, Pos('|', AFullText) + 1, Length(AFullText));
  end;
end;

function TSampleForm.getTextForBookmark(bookmarkName: String; document: IdxRichEditDocument): String;
var
  bookmark: IdxRichEditBookmark;
  bookmarkRange: IdxRichEditDocumentRange;
  fragmentOpt: TdxRichEditTextFragmentOptions;
  para: IdxRichEditParagraph;
  paraRange: IdxRichEditDocumentRange;
begin
  bookmark := document.bookmarks.Items[bookmarkName];
  if not assigned(bookmark) then begin
    result := '';
  end;

  bookmarkRange := bookmark.range;

  para := document.Paragraphs.Get(bookmarkRange.Start);
  paraRange := para.range;

  fragmentOpt := TdxRichEditTextFragmentOptions.create;
  fragmentOpt.AllowExtendingDocumentRange := true;
  result := document.GetText(paraRange, fragmentOpt);
end;

initialization

finalization

end.
