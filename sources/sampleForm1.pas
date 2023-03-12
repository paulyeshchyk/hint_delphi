unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, dxCore, dxCoreClasses, dxRichEdit.NativeApi,
  dxRichEdit.Types, dxRichEdit.PlainText, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxRichEdit.DocumentServer, dxGDIPlusAPI, dxGDIPlusClasses, dxRichEdit.Options,
  dxRichEdit.Control, dxRichEdit.Control.SpellChecker, dxRichEdit.Dialogs.EventArgs, dxRichEdit.Platform.Win.Control,
  dxRichEdit.Control.Core,
  dxRichEdit.Api.Paragraphs,
  dxRichEdit.Api.Hyperlinks,
  dxRichEdit.Api.NativeDocument, dxScreenTip, cxClasses, dxCustomHint, cxHint, dxFormattedLabel, cxLabel, Vcl.ExtCtrls,

  OPP.System,
  OPP.Hint,
  OPP.VCL.Controls,
  OPP.dxRichEdit, Vcl.ComCtrls;

type
  TSampleForm = class(TForm)
    Button1: TButton;
    LinkToText: TCheckBox;
    Edit1: TEdit;
    docServer: TdxRichEditDocumentServer;
    RichEdit1: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  fStream: TStringStream;
begin
  hintServer := OPPRichEditHintServer.create;
  loadResult := hintServer.loadFromFile(filepath);
  if loadResult.error = nil then
  begin
    fHints := hintServer.GetHints(self);
    fRTF := fHints.First.data.rtf;
    fStream := TStringStream.Create(fRTF);
    self.RichEdit1.Lines.LoadFromStream(fStream);
  end;
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

function TSampleForm.getTextForBookmark(bookmarkName: String; document: IdxRichEditDocument): String;
var
  bookmark: IdxRichEditBookmark;
  bookmarkRange: IdxRichEditDocumentRange;
  fragmentOpt: TdxRichEditTextFragmentOptions;
  para: IdxRichEditParagraph;
  paraRange: IdxRichEditDocumentRange;
begin
  bookmark := document.bookmarks.Items[bookmarkName];
  if not assigned(bookmark) then
  begin
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
