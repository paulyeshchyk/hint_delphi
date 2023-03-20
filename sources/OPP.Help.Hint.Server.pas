unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs, System.SysUtils, System.Classes, System.Generics.Collections,
  VCL.Controls, OPP.Help.Hint, OPP.VCL.Controls,
  //
  OPP.Help.Hint.Mapping,
  OPP.Help.Hint.Reader;

type
  TOPPHelpHintLoadCompletion = reference to procedure(loadedHints: TList<TOPPHelpHint>);

  TOPPHelpHintServerOnHintTextsFilenameRequest = reference to function(): string;

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
    fHintMapSet: TOPPHelpHintMapSet;

    fHintDataReaders: TDictionary<String, IOPPHelpHintDataReader>;
    fOnHintTextsFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure reloadConfigurationIfNeed();
    function findOrCreateReader(AIdentifier: String): IOPPHelpHintDataReader;
    function getReader(AFileName: String):IOPPHelpHintDataReader;

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

uses
  OPP.Help.Hint.Mapping.Filereader;

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
  fHintDataReaders := TDictionary<String, IOPPHelpHintDataReader>.Create();
  fHintMapSet := TOPPHelpHintMapSet.Create();
end;

destructor TOPPHelpHintServer.Destroy;
begin
  fHintDataReaders := nil;
  inherited Destroy;
end;

{ private }

procedure TOPPHelpHintServer.reloadConfigurationIfNeed();
var
  fFileName: string;
  fOPPHelpHintMapJSONReadCallback: TOPPHelpHintMapJSONReadCallback;
begin
  if fLoaded then
    exit;

  if not Assigned(fOnHintTextsFileNameRequest) then
    exit;

  fOPPHelpHintMapJSONReadCallback := procedure(AList: TList<TOPPHelpHintMap>; error: Exception)
    begin
      if Assigned(AList) then
        fHintMapSet.list.AddRange(AList);

      self.fLoaded := true;
    end;

  fFileName := fOnHintTextsFileNameRequest();
  TOPPHelpHintMap.readJSON(fFileName, fOPPHelpHintMapJSONReadCallback);
end;

{ public }

function TOPPHelpHintServer.findOrCreateReader(AIdentifier: String): IOPPHelpHintDataReader;
var
  fMap : TOPPHelpHintMap;
  fReader: IOPPHelpHintDataReader;
begin
  result := nil;

  fMap := fHintMapSet.GetMap(AIdentifier);
  if Assigned(fMap) then
  begin
    fReader := getReader(fMap.filename);
    if not Assigned(fReader) then begin
      fReader := TOPPHelpRichtextHintReader.Create;
      fReader.loadData(fMap.filename);
      fHintDataReaders.add(fMap.filename, fReader);
    end;
    result := fReader;
  end;
end;


function TOPPHelpHintServer.getReader(AFileName: String):IOPPHelpHintDataReader;
var
  Mapping: IOPPHelpHintDataReader;
begin
  try
    Mapping := nil;
    try
      fHintDataReaders.TryGetValue(AFileName, Mapping);
    except
      on e: Exception do
      begin
        //
      end;
    end;

  finally
    result := Mapping;
  end;
end;

function TOPPHelpHintServer.GetHintData(identifier: TOPPHintIdentifierType): TOPPHelpHintData;
var
  fReader: IOPPHelpHintDataReader;
begin

  self.reloadConfigurationIfNeed();
  if not fLoaded then
    exit;

  fReader := findOrCreateReader(identifier);
  if Assigned(fReader) then
  begin

    result := fReader.FindHintDataForBookmarkIdentifier(identifier);
  end;
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

  self.reloadConfigurationIfNeed();
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  result := TList<TOPPHelpHint>.Create;
  for fHintMeta in hintsMetaList do
  begin
    fHint := GetHint(fHintMeta);
    if not fHint.data.isEmpty() then
    begin
      result.add(fHint);
    end;
  end;
  completion(result);
end;

procedure TOPPHelpHintServer.GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion);
var
  fHintMetaList: TList<TOPPHelpHintMeta>;
  fHintMeta: TOPPHelpHintMeta;
  id: String;
  fMap: TOPPHelpHintMap;
  fReader: TOPPHelpRichtextHintReader;
begin

  self.reloadConfigurationIfNeed();
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fHintMetaList := Control.GetControlHintsMeta();
  for fHintMeta in fHintMetaList do
  begin
    id := fHintMeta.hintIdentifier;

    self.findOrCreateReader(id);
  end;

  self.GetHints(fHintMetaList, completion);

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
