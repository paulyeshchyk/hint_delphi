﻿unit OPP.Help.Hint.Reader;

interface

uses
  System.SysUtils,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  OPP.Help.Hint;

type
  IOPPHelpHintReader = interface

    /// <summary>
    /// Загружает файл подсказок
    ///
    /// </summary>
    function loadData(AFileName: String): TOPPHelpHintServerLoadResultType;

    function FindHintDataForBookmarkIdentifier(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
  end;

  TOPPHelpRichtextHintReader = class(TInterfacedObject, IOPPHelpHintReader)
  const
    fDocumentFormat: TdxRichEditDocumentFormat = TdxRichEditDocumentFormat.rtf;
  private
    fLoaded: Boolean;
    fRichEditControl: TdxRichEditControl;
    function getDocument(): IdxRichEditDocument;
    function GetBookmark(bookmarkId: TOPPHintIdentifierType): IdxRichEditBookmark;
    function GetParagraph(bookmark: IdxRichEditBookmark): IdxRichEditParagraph; overload;
    function GetParagraph(position: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function GetPlainText(paragraph: IdxRichEditParagraph): String;
    function GetRichText(paragraph: IdxRichEditParagraph): String;
    property Document: IdxRichEditDocument read getDocument;
  public
    constructor Create;
    destructor Destroy; override;
    function loadData(AFileName: String): TOPPHelpHintServerLoadResultType;
    function FindHintDataForBookmarkIdentifier(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
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

function TOPPHelpRichtextHintReader.FindHintDataForBookmarkIdentifier(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
var
  bookmark: IdxRichEditBookmark;
  paragraph: IdxRichEditParagraph;
begin

  bookmark := self.GetBookmark(identifier);
  paragraph := self.GetParagraph(bookmark);
  result.text := self.GetPlainText(paragraph);
  result.rtf := self.GetRichText(paragraph);
end;

function TOPPHelpRichtextHintReader.getDocument(): IdxRichEditDocument;
begin
  result := nil;
  if not fLoaded then
    exit;
  result := fRichEditControl.Document;
end;

function TOPPHelpRichtextHintReader.GetBookmark(bookmarkId: TOPPHintIdentifierType): IdxRichEditBookmark;
begin
  result := nil;
  if Length(bookmarkId) = 0 then
    exit;
  result := Document.bookmarks.items[bookmarkId];
end;

function TOPPHelpRichtextHintReader.GetParagraph(bookmark: IdxRichEditBookmark): IdxRichEditParagraph;
begin
  result := nil;
  if not Assigned(bookmark) then
    exit;
  result := Document.Paragraphs.Get(bookmark.range.Start);
end;

function TOPPHelpRichtextHintReader.GetParagraph(position: IdxRichEditDocumentPosition): IdxRichEditParagraph;
begin
  result := nil;
  if not Assigned(position) then
    exit;
  result := Document.Paragraphs.Get(position);
end;

function TOPPHelpRichtextHintReader.GetPlainText(paragraph: IdxRichEditParagraph): String;
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

function TOPPHelpRichtextHintReader.GetRichText(paragraph: IdxRichEditParagraph): String;
begin
  result := '';
  if not Assigned(paragraph) then
    exit;
  if not fLoaded then
    exit;
  result := Document.GetRtfText(paragraph.range);
end;

end.