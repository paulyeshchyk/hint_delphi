unit OPP.Help.HintServer;

interface

uses
  System.SyncObjs,
  System.SysUtils, System.Classes, System.Generics.Collections,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  VCL.Controls,
  OPP.Help.HintMapping,
  OPP.VCL.Controls;

const
  filepath: String = 'docs\gulfstream_manual_rtf.rtf';

type
  IOPPHelpHintServer = interface
    function GetHint(hintMeta: TOPPHintMeta): TOPPHint;
    function GetHints(Control: TControl): TList<TOPPHint>; overload;
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHint>; overload;
  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fLoaded: Boolean;
    fRichEditControl: TdxRichEditControl;

  const
    fDocumentFormat: TdxRichEditDocumentFormat = TdxRichEditDocumentFormat.rtf;

    function getDocument(): IdxRichEditDocument;

  public

    property document: IdxRichEditDocument read getDocument;
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// ��������� ���� ���������� ����������
    ///
    /// </summary>
    /// <remarks> ����������� �������� ������ ���� � ������� rtf</remarks>
    function loadFromFile(AFileName: String): TOPPHintServerLoadResultType;

    /// <summary>
    /// ���������� ������ ���������, ���������� ��� ������ ���������������, ������ �� ����������.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHint>; overload;

    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHintData;

    /// <summary>
    /// ���������� ��������� ��� ����������, ���������� �������� ������� � ��������� hintMeta.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHint(hintMeta: TOPPHintMeta): TOPPHint; overload;

    /// <summary>
    /// ���������� ������ ���������, ���������� ��� ����������, ���������� � ��������� Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHints(Control: TControl): TList<TOPPHint>; overload;
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

var
  fLock: TCriticalSection;
  fHelpHintServer: IOPPHelpHintServer;

function helpHintServer: IOPPHelpHintServer;
begin
  fLock.Acquire;
  try
    if not Assigned(fHelpHintServer) then begin
      fHelpHintServer := TOPPHelpHintServer.Create;
    end;
    result := fHelpHintServer;
  finally
    fLock.Release;
  end;
end;

constructor TOPPHelpHintServer.Create;
begin
  fRichEditControl := TdxRichEditControl.Create(nil);
  fLoaded := (loadFromFile(filepath).error = nil)
end;

destructor TOPPHelpHintServer.Destroy;
begin
  FreeAndNil(fRichEditControl);
  inherited Destroy;
end;

{ public }

function TOPPHelpHintServer.loadFromFile(AFileName: String): TOPPHintServerLoadResultType;
var
  loadResult: TOPPHintServerLoadResultType;
begin
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

function TOPPHelpHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHintData;
var
  bookmark: IdxRichEditBookmark;
  fragmentOpt: TdxRichEditTextFragmentOptions;
  paragraph: IdxRichEditParagraph;
begin
  if not fLoaded then
    exit;

  bookmark := document.bookmarks.items[identifier];
  if not Assigned(bookmark) then
    exit;

  paragraph := document.Paragraphs.Get(bookmark.range.Start);

  fragmentOpt := TdxRichEditTextFragmentOptions.Create;
  fragmentOpt.AllowExtendingDocumentRange := true;

  result.text := document.GetText(paragraph.range, fragmentOpt);
  result.rtf := document.GetRtfText(paragraph.range);
end;

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHintMeta): TOPPHint;
begin
  result.data := GetHintData(hintMeta.hintIdentifier);
  result.meta := hintMeta;
end;

function TOPPHelpHintServer.GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHint>;
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

function TOPPHelpHintServer.GetHints(Control: TControl): TList<TOPPHint>;
var
  fHintsMeta: TList<TOPPHintMeta>;
begin
  fHintsMeta := Control.GetControlHintsMeta();
  result := self.GetHints(fHintsMeta);
end;

{ private }

function TOPPHelpHintServer.getDocument(): IdxRichEditDocument;
begin
  result := fRichEditControl.document;
end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
