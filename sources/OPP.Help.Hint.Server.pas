unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs,
  System.SysUtils, System.Classes, System.Generics.Collections,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  VCL.Controls,
  OPP.Help.Hint,
  OPP.Help.Hint.Document,
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
    fHintDocument: IOPPHelpHintDocument;
    procedure reloadIfNeed();

  public
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Возвращает список подсказок, применимых для списка идентификаторов, взятых из компонента.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHints(hintsMetaList: TOPPHintIdList): TList<TOPPHelpHint>; overload;

    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;

    /// <summary>
    /// Возвращает подсказку для компонента, метаданные которого указаны в параметре hintMeta.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint; overload;

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
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
    if not Assigned(fHelpHintServer) then
    begin
      fHelpHintServer := TOPPHelpHintServer.Create;
    end;
    result := fHelpHintServer;
  finally
    fLock.Release;
  end;
end;

constructor TOPPHelpHintServer.Create;
begin
  fHintDocument := TOPPHelpHintDocument.Create;
end;

destructor TOPPHelpHintServer.Destroy;
begin
  fHintDocument := nil;
  inherited Destroy;
end;

{ private }

procedure TOPPHelpHintServer.reloadIfNeed();
begin
  if fLoaded then
    exit;

  fLoaded := (fHintDocument.loadFromFile(filepath).error = nil);
end;

{ public }

function TOPPHelpHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
begin

  self.reloadIfNeed();
  if not fLoaded then
    exit;

  result := fHintDocument.GetHintData(identifier);
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

  result := nil;

  self.reloadIfNeed();
  if not fLoaded then
    exit;

  result := TList<TOPPHelpHint>.Create;
  for fHintMeta in hintsMetaList do
  begin
    fHint := GetHint(fHintMeta);
    if not fHint.data.isEmpty() then
    begin
      result.add(fHint);
    end;
  end;
end;

function TOPPHelpHintServer.GetHints(Control: TControl): TList<TOPPHelpHint>;
var
  fHintsMeta: TList<TOPPHelpHintMeta>;
begin
  result := nil;
  self.reloadIfNeed();
  if not fLoaded then
    exit;

  fHintsMeta := Control.GetControlHintsMeta();
  result := self.GetHints(fHintsMeta);
end;

{ private }

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
