unit OPP.dxRichEdit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  VCL.Controls,
  OPP.Hint,
  OPP.VCL.Controls;

type
  OPPRichEditHintServer = class
  private
    fLoaded: Boolean;
    fFileName: String;
    fRichEditControl: TdxRichEditControl;

  const
    fDocumentFormat: TdxRichEditDocumentFormat = TdxRichEditDocumentFormat.rtf;

    function getDocument(): IdxRichEditDocument;

  public

    property document: IdxRichEditDocument read getDocument;
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    function loadFromFile(AFileName: String): TOPPHintServerLoadResultType;
    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHintData;
    function GetHint(hintMeta: TOPPHintMeta): TOPPHint; overload;
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHint>; overload;
    function GetHints(Control: TControl): TList<TOPPHint>; overload;
  end;

implementation

constructor OPPRichEditHintServer.Create;
begin
  fLoaded := false;
  fFileName := '';
  fRichEditControl := TdxRichEditControl.Create(nil);
end;

destructor OPPRichEditHintServer.Destroy;
begin
  FreeAndNil(fRichEditControl);
  inherited Destroy;
end;

{ public }

function OPPRichEditHintServer.loadFromFile(AFileName: String): TOPPHintServerLoadResultType;
var
  loadResult: TOPPHintServerLoadResultType;
begin
  fFileName := AFileName;
  try
    fRichEditControl.LoadDocument(AFileName, fDocumentFormat);
    loadResult.error := nil;
    fLoaded := true;
  except
    on E: Exception do begin
      loadResult.error := Exception.Create(E.Message);
    end;
  end;
  result := loadResult;
end;

function OPPRichEditHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHintData;
var
  bookmark: IdxRichEditBookmark;
  fragmentOpt: TdxRichEditTextFragmentOptions;
  paragraph: IdxRichEditParagraph;
begin

  if not fLoaded then
    exit;

  bookmark := document.bookmarks.items[identifier];
  if not assigned(bookmark) then
    exit;

  paragraph := document.Paragraphs.Get(bookmark.range.Start);

  fragmentOpt := TdxRichEditTextFragmentOptions.Create;
  fragmentOpt.AllowExtendingDocumentRange := true;

  result.text := document.GetText(paragraph.range, fragmentOpt);
  result.rtf := document.GetRtfText(paragraph.range);
end;

function OPPRichEditHintServer.GetHint(hintMeta: TOPPHintMeta): TOPPHint;
begin
  result.data := GetHintData(hintMeta.hintIdentifier);
  result.meta := hintMeta;
end;

function OPPRichEditHintServer.GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHint>;
var
  fHintMeta: TOPPHintMeta;
  fHint: TOPPHint;
begin
  result := TList<TOPPHint>.Create;
  for fHintMeta in hintsMetaList do begin
    fHint := GetHint(fHintMeta);
    if not fHint.data.isEmpty() then begin
      result.add(fHint);
    end;
  end;
end;

function OPPRichEditHintServer.GetHints(Control: TControl): TList<TOPPHint>;
var
  fHintsMeta: TList<TOPPHintMeta>;
begin
  fHintsMeta := Control.GetControlHintsMeta();
  result := self.GetHints(fHintsMeta);
end;

{ private }

function OPPRichEditHintServer.getDocument(): IdxRichEditDocument;
begin
  result := fRichEditControl.document;
end;

end.
