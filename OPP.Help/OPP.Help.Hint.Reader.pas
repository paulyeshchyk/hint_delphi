unit OPP.Help.Hint.Reader;

interface

uses
  System.SysUtils,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  OPP.Help.Nonatomic,
  OPP.Help.Predicate,
  OPP.Help.Hint;

type
  IOPPHelpHintDataReader = interface

    /// <summary>
    /// Загружает файл подсказок
    ///
    /// </summary>
    function loadData(AFileName: String): TOPPHelpHintServerLoadResultType;

    function FindHintDataForBookmarkIdentifier(APredicate: TOPPHelpPredicate): TOPPHelpHintData;
  end;

  TOPPHelpRichtextHintReader = class(TInterfacedObject, IOPPHelpHintDataReader)
  const
    fDocumentFormat: TdxRichEditDocumentFormat = TdxRichEditDocumentFormat.rtf;
  private
    fLoaded: Boolean;
    fRichEditControl: TdxRichEditControl;
    function getDocument(): dxRichEdit.NativeAPI.IdxRichEditDocument;
    function GetBookmark(AHintIdentifier: TOPPHelpMetaIdentifierType): dxRichEdit.NativeAPI.IdxRichEditBookmark;
    function GetParagraph(bookmark: dxRichEdit.NativeAPI.IdxRichEditBookmark): dxRichEdit.NativeAPI.IdxRichEditParagraph; overload;
    function GetParagraph(position: dxRichEdit.NativeAPI.IdxRichEditDocumentPosition): dxRichEdit.NativeAPI.IdxRichEditParagraph; overload;
    function GetPlainText(paragraph: dxRichEdit.NativeAPI.IdxRichEditParagraph): String;
    function GetRichText(paragraph: dxRichEdit.NativeAPI.IdxRichEditParagraph): String;
    property Document: dxRichEdit.NativeAPI.IdxRichEditDocument read getDocument;
  public
    constructor Create;
    destructor Destroy; override;
    function loadData(AFileName: String): TOPPHelpHintServerLoadResultType;
    function FindHintDataForBookmarkIdentifier(APredicate: TOPPHelpPredicate): TOPPHelpHintData;
  end;

implementation

constructor TOPPHelpRichtextHintReader.Create;
begin
  fRichEditControl := TdxRichEditControl.Create(nil);
end;

destructor TOPPHelpRichtextHintReader.Destroy;
begin
  FreeAndNil(fRichEditControl);
  inherited;
end;

function TOPPHelpRichtextHintReader.loadData(AFileName: string): TOPPHelpHintServerLoadResultType;
var
  loadResult: TOPPHelpHintServerLoadResultType;
begin
  try
    fRichEditControl.LoadDocument(AFileName, fDocumentFormat);
    loadResult.error := nil;
    fLoaded := true;
  except
    on E: Exception do
    begin
      loadResult.error := Exception.Create(E.Message);
    end;
  end;
  result := loadResult;

end;

function TOPPHelpRichtextHintReader.FindHintDataForBookmarkIdentifier(APredicate: TOPPHelpPredicate): TOPPHelpHintData;
var
  bookmark: IdxRichEditBookmark;
  paragraph: IdxRichEditParagraph;
begin

  case APredicate.keywordType of
    ktBookmark:
      begin
        bookmark := self.GetBookmark(APredicate.value);
        paragraph := self.GetParagraph(bookmark);
        result.text := self.GetPlainText(paragraph);
        result.rtf := self.GetRichText(paragraph);
      end;
    ktAny:
      begin
        bookmark := self.GetBookmark(APredicate.value);
        paragraph := self.GetParagraph(bookmark);
        result.text := self.GetPlainText(paragraph);
        result.rtf := self.GetRichText(paragraph);
      end;
    ktSearch:
      begin
        bookmark := self.GetBookmark(APredicate.value);
        paragraph := self.GetParagraph(bookmark);
        result.text := self.GetPlainText(paragraph);
        result.rtf := self.GetRichText(paragraph);
      end;
    ktPage:
      begin
        bookmark := self.GetBookmark(APredicate.value);
        paragraph := self.GetParagraph(bookmark);
        result.text := self.GetPlainText(paragraph);
        result.rtf := self.GetRichText(paragraph);
      end;
  end;

end;

function TOPPHelpRichtextHintReader.getDocument(): dxRichEdit.NativeAPI.IdxRichEditDocument;
begin
  result := nil;
  if not fLoaded then
    exit;
  result := fRichEditControl.Document;
end;

function TOPPHelpRichtextHintReader.GetBookmark(AHintIdentifier: TOPPHelpMetaIdentifierType): dxRichEdit.NativeAPI.IdxRichEditBookmark;
begin
  result := nil;
  if Length(AHintIdentifier) = 0 then
    exit;
  result := Document.bookmarks.items[AHintIdentifier];
end;

function TOPPHelpRichtextHintReader.GetParagraph(bookmark: dxRichEdit.NativeAPI.IdxRichEditBookmark): dxRichEdit.NativeAPI.IdxRichEditParagraph;
begin
  result := nil;
  if not Assigned(bookmark) then
    exit;
  result := Document.Paragraphs.Get(bookmark.range.Start);
end;

function TOPPHelpRichtextHintReader.GetParagraph(position: dxRichEdit.NativeAPI.IdxRichEditDocumentPosition): dxRichEdit.NativeAPI.IdxRichEditParagraph;
begin
  result := nil;
  if not Assigned(position) then
    exit;
  result := Document.Paragraphs.Get(position);
end;

function TOPPHelpRichtextHintReader.GetPlainText(paragraph: dxRichEdit.NativeAPI.IdxRichEditParagraph): String;
var
  fragmentOptions: TdxRichEditTextFragmentOptions;
begin
  result := '';
  if not Assigned(paragraph) then
    exit;
  if not fLoaded then
    exit;
  fragmentOptions := TdxRichEditTextFragmentOptions.Create;
  fragmentOptions.AllowExtendingDocumentRange := true;
  result := Document.GetText(paragraph.range, fragmentOptions);
end;

function TOPPHelpRichtextHintReader.GetRichText(paragraph: dxRichEdit.NativeAPI.IdxRichEditParagraph): String;
begin
  result := '';
  if not Assigned(paragraph) then
    exit;
  if not fLoaded then
    exit;
  result := Document.GetRtfText(paragraph.range);
end;

end.
