unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs,
  System.SysUtils, System.Classes, System.Generics.Collections,
  dxRichEdit.Control, dxRichEdit.NativeAPI,
  VCL.Controls,
  OPP.Help.Hint,
  OPP.VCL.Controls,
  //
  OPP.Help.Hint.Reader;

type
  TOPPHelpHintLoadCompletion                       = reference to procedure(loadedHints: TList<TOPPHelpHint>);
  TOPPHelpHintServerOnHintTextsFilenameRequest = reference to function(): String;

  IOPPHelpHintServer = interface

    /// <summary>
    /// Возвращает подсказку для компонента, метаданные которого указаны в параметре hintMeta.
    ///
    /// </summary>
    /// <remarks> </remarks>
    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint;

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    procedure GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion); overload;

    /// <summary>
    /// Возвращает список подсказок, применимых для списка идентификаторов, взятых из компонента.
    ///
    /// </summary>
    /// <remarks> </remarks>
    procedure GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;

    function getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
    property OnHintTextsFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest read getOnHintTextsFileNameRequest write setOnHintTextsFileNameRequest;

  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fLoaded: Boolean;
    fHintReader: IOPPHelpHintReader;
    fOnHintTextsFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure reloadIfNeed();

  public
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    function GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
    procedure GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion); overload;
    procedure GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;

    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint; overload;

    function getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
    property OnHintTextsFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest read fOnHintTextsFileNameRequest write fOnHintTextsFileNameRequest;
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
  fHintReader := TOPPHelpRichtextHintReader.Create;
end;

destructor TOPPHelpHintServer.Destroy;
begin
  fHintReader := nil;
  inherited Destroy;
end;

{ private }

procedure TOPPHelpHintServer.reloadIfNeed();
var
  fFileName: String;
begin
  if fLoaded then
    exit;

  if not Assigned(fOnHintTextsFileNameRequest) then
    exit;

  fFileName := fOnHintTextsFileNameRequest();

  fLoaded := (fHintReader.loadData(fFileName).error = nil);
end;

{ public }

function TOPPHelpHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
begin

  self.reloadIfNeed();
  if not fLoaded then
    exit;

  result := fHintReader.FindHintDataForBookmarkIdentifier(identifier);
end;

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint;
begin
  result.data := GetHintData(hintMeta.hintIdentifier);
  result.meta := hintMeta;
end;

procedure TOPPHelpHintServer.GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion);
var
  fHintMeta: TOPPHelpHintMeta;
  fHint: TOPPHelpHint;
  result: TList<TOPPHelpHint>;
begin

  self.reloadIfNeed();
  if not fLoaded then begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  result := TList<TOPPHelpHint>.Create;
  for fHintMeta in hintsMetaList do begin
    fHint := GetHint(fHintMeta);
    if not fHint.data.isEmpty() then begin
      result.add(fHint);
    end;
  end;
  completion(result);
end;

procedure TOPPHelpHintServer.GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion);
var
  fHintsMeta: TList<TOPPHelpHintMeta>;
begin

  self.reloadIfNeed();
  if not fLoaded then begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fHintsMeta := Control.GetControlHintsMeta();

  self.GetHints(fHintsMeta, completion);

end;

function TOPPHelpHintServer.getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
begin
  result := fOnHintTextsFileNameRequest;
end;

procedure TOPPHelpHintServer.setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
begin
  fOnHintTextsFileNameRequest := value;
end;

{ private }

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
