unit OPP.Help.Hint.Document;

interface

uses
  System.SysUtils,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  OPP.Help.Hint;

type

  IOPPHelpHintDocument = interface
    function loadFromFile(AFileName: String): TOPPHelpHintServerLoadResultType;
    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
  end;

  TOPPHelpHintDocument = class(TInterfacedObject, IOPPHelpHintDocument)
  const
    fDocumentFormat: TdxRichEditDocumentFormat = TdxRichEditDocumentFormat.rtf;
  private
    fLoaded: Boolean;
    fRichEditControl: TdxRichEditControl;
    function getDocument(): IdxRichEditDocument;
    property Document: IdxRichEditDocument read getDocument;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Загружает файл справочной информации
    ///
    /// </summary>
    /// <remarks> Загружаемый документ должен быть в формате rtf</remarks>
    function loadFromFile(AFileName: String): TOPPHelpHintServerLoadResultType;

    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
  end;

implementation

constructor TOPPHelpHintDocument.Create;
begin
  inherited Create;
  fRichEditControl := TdxRichEditControl.Create(nil);
end;

destructor TOPPHelpHintDocument.Destroy;
begin
  FreeAndNil(fRichEditControl);
  inherited Destroy;
end;

function TOPPHelpHintDocument.loadFromFile(AFileName: String): TOPPHelpHintServerLoadResultType;
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

function TOPPHelpHintDocument.getDocument(): IdxRichEditDocument;
begin
  result := fRichEditControl.Document;
end;

function TOPPHelpHintDocument.GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
var
  bookmark: IdxRichEditBookmark;
  fragmentOptions: TdxRichEditTextFragmentOptions;
  paragraph: IdxRichEditParagraph;
begin

  if not fLoaded then
    exit;

  bookmark := Document.bookmarks.items[identifier];
  if not Assigned(bookmark) then
    exit;

  paragraph := Document.Paragraphs.Get(bookmark.range.Start);

  fragmentOptions := TdxRichEditTextFragmentOptions.Create;
  fragmentOptions.AllowExtendingDocumentRange := true;

  result.text := Document.GetText(paragraph.range, fragmentOptions);
  result.rtf := Document.GetRtfText(paragraph.range);
end;

end.
