unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs,
  System.SysUtils, System.Classes, System.Generics.Collections,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  VCL.Controls,
  OPP.Help.Hint,
  OPP.VCL.Controls;

const
  filepath: String = 'docs\gulfstream_manual_rtf.rtf';

type
  IOPPHelpHintServer = interface
    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint;
    function GetHints(Control: TControl): TList<TOPPHelpHint>; overload;
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHelpHint>; overload;
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
    function loadFromFile(AFileName: String): TOPPHelpHintServerLoadResultType;

    /// <summary>
    /// ���������� ������ ���������, ���������� ��� ������ ���������������, ������ �� ����������.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHelpHint>; overload;

    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;

    /// <summary>
    /// ���������� ��������� ��� ����������, ���������� �������� ������� � ��������� hintMeta.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint; overload;

    /// <summary>
    /// ���������� ������ ���������, ���������� ��� ����������, ���������� � ��������� Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHints(Control: TControl): TList<TOPPHelpHint>; overload;
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

function TOPPHelpHintServer.loadFromFile(AFileName: String): TOPPHelpHintServerLoadResultType;
var
  loadResult: TOPPHelpHintServerLoadResultType;
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

function TOPPHelpHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
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

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint;
begin
  result.data := GetHintData(hintMeta.hintIdentifier);
  result.meta := hintMeta;
end;

function TOPPHelpHintServer.GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHelpHint>;
var
  fHintMeta: TOPPHelpHintMeta;
  fHint: TOPPHelpHint;
begin
  result := TList<TOPPHelpHint>.Create;
  for fHintMeta in hintsMetaList do begin
    fHint := GetHint(fHintMeta);
    if not fHint.data.isEmpty() then begin
      result.add(fHint);
    end;
  end;
end;

function TOPPHelpHintServer.GetHints(Control: TControl): TList<TOPPHelpHint>;
var
  fHintsMeta: TList<TOPPHelpHintMeta>;
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
