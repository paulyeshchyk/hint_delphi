unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs, System.SysUtils, System.Classes, System.Generics.Collections,
  System.TypInfo,
  WinAPI.Windows,
  VCL.Controls,
  //
  OPP.Help.Hint, OPP.Help.Component.Enumerator,
  //
  OPP.Help.Nonatomic,
  OPP.Help.System.Str,
  OPP.Help.Predicate,
  OPP.Help.Hint.Mapping,
  OPP.Help.Hint.Reader,
  OPP.Help.Meta;

type
  TOPPHelpHintLoadCompletion = reference to procedure(loadedHints: TList<TOPPHelpHint>);
  TOPPHelpMapGenerationCompletion = reference to procedure(AList: TList<TOPPHelpHintMap>);
  TOPPHelpHintServerOnGetMetaFactory = reference to function(): IOPPHelpMetaFactory;

  TOPPHelpHintMappingRequest = class
  private
    fMappingFileName: String;
    fControl: TControl;
    fOnGetHintFactory: TOPPHelpHintServerOnGetMetaFactory;
  public
    property MappingFileName: String read fMappingFileName write fMappingFileName;
    property Control: TControl read fControl write fControl;
    property OnGetHintFactory: TOPPHelpHintServerOnGetMetaFactory read fOnGetHintFactory write fOnGetHintFactory;
  end;

  TOPPHelpHintMappingLoadRequest = class(TOPPHelpHintMappingRequest)
  private
    fReader: IOPPHelpHintDataReader;
  public
    property Reader: IOPPHelpHintDataReader read fReader write fReader;
  end;

  TOPPHelpHintMappingSaveRequest = class(TOPPHelpHintMappingRequest)
  private
    fDefaultPredicateFileName: String;
  public
    property DefaultPredicateFileName: String read fDefaultPredicateFileName write fDefaultPredicateFileName;
  end;

  IOPPHelpHintServer = interface

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    procedure GetHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure GenerateMap(ARequest: TOPPHelpHintMappingSaveRequest; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);
  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fLoaded: Boolean;
    fHintMapSet: TOPPHelpHintMapSet;

    fHintMetaDict: TDictionary<TSymbolName, String>;

    fHintDataReaders: TDictionary<String, IOPPHelpHintDataReader>;
    procedure GetHints(ARequest: TOPPHelpHintMappingLoadRequest; hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;
    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint; overload;
    function GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
    procedure reloadConfigurationIfNeed(filename: String);
    function findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
    function getReader(AFileName: String): IOPPHelpHintDataReader;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure GenerateMap(ARequest: TOPPHelpHintMappingSaveRequest; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);

    property loaded: Boolean read fLoaded;
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

uses
  OPP.Help.Hint.Mapping.Filereader, OPP.Help.System.Error,
  OPP.Help.Log;

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
  fHintMetaDict := TDictionary<TSymbolName, String>.Create();
end;

destructor TOPPHelpHintServer.Destroy;
begin
  fHintDataReaders := nil;
  inherited Destroy;
end;

{ private }

procedure TOPPHelpHintServer.reloadConfigurationIfNeed(filename: String);
var
  fOPPHelpHintMapJSONReadCallback: TOPPHelpHintMapJSONReadCallback;
begin
  if fLoaded then
    exit;

  if Length(filename) = 0 then
    exit;

  eventLogger.Log('will load config');

  fOPPHelpHintMapJSONReadCallback := procedure(AList: TList<TOPPHelpHintMap>; Error: Exception)
    begin

      self.fLoaded := true;

      if Assigned(Error) then
      begin
        Error.Log();
        exit;
      end;
      fHintMapSet.AddMaps(AList);
    end;

  TOPPHelpHintMap.readJSON(filename, fOPPHelpHintMapJSONReadCallback);

end;

{ public }

// TODO: add callback where hintmap and reader be returned
// reader will take predicate from hintmap and then it should run search using predicate
function TOPPHelpHintServer.findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
var
  fMap: TOPPHelpHintMap;
  fPredicate: TOPPHelpPredicate;
  outStr: String;
  mapWasFound: Boolean;
begin
  result := nil;

  fMap := fHintMapSet.GetMap(AMetaIdentifier);
  mapWasFound := Assigned(fMap);

  if not mapWasFound then
  begin
    outStr := Format('map not found for %s', [AMetaIdentifier]);
    eventLogger.Log(outStr, lmWarning);
    exit;
  end;

  outStr := Format('map was found for %s', [AMetaIdentifier]);
  eventLogger.Log(outStr, lmInfo);

  fPredicate := fMap.Predicate;
  if not Assigned(fPredicate) then
  begin
    eventLogger.Log('!!! PREDICATE NOT FOUND!!!', lmError);
    exit;
  end;

  result := getReader(fMap.Predicate.filename);
  if Assigned(result) then
    exit;

  result := TOPPHelpRichtextHintReader.Create;
  result.loadData(fMap.Predicate.filename);
  fHintDataReaders.Add(fMap.Predicate.filename, result);
end;

function TOPPHelpHintServer.getReader(AFileName: String): IOPPHelpHintDataReader;
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
        e.Log();
      end;
    end;

  finally
    result := Mapping;
  end;
end;

function TOPPHelpHintServer.GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
var
  fReader: IOPPHelpHintDataReader;
  fHintMap: TOPPHelpHintMap;
begin

  fReader := findOrCreateReader(AHintIdentifier);
  if Assigned(fReader) then
  begin

    fHintMap := fHintMapSet.GetMap(AHintIdentifier);
    if Assigned(fHintMap) then
      result := fReader.FindHintDataForBookmarkIdentifier(fHintMap.Predicate);
  end;
end;

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
begin
  result.data := GetHintData(hintMeta.identifier);
  result.Meta := hintMeta;
end;

procedure TOPPHelpHintServer.GetHints(ARequest: TOPPHelpHintMappingLoadRequest; hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion);
var
  fHintMeta: TOPPHelpMeta;
  fHint: TOPPHelpHint;
  result: TList<TOPPHelpHint>;
begin

  self.reloadConfigurationIfNeed(ARequest.MappingFileName);
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
      result.Add(fHint);
    end;
  end;
  completion(result);
end;

procedure TOPPHelpHintServer.GetHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion);
var
  fChildrenHelpMetaList: TList<TOPPHelpMeta>;
  fChildHelpMeta: TOPPHelpMeta;
  fMetaIdentifier: TOPPHelpHintMapIdentifier;
  fFactory: IOPPHelpMetaFactory;
begin

  eventLogger.Log('Hintserver.gethints(Control)');

  self.reloadConfigurationIfNeed(ARequest.MappingFileName);
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  if not Assigned(ARequest.OnGetHintFactory) then
  begin
    eventLogger.Log('GetHints - OnGetHintFactory is not defined', lmError);
    exit;
  end;

  fFactory := ARequest.OnGetHintFactory();

  fChildrenHelpMetaList := fFactory.GetChildrenHelpMeta(ARequest.Control);
  for fChildHelpMeta in fChildrenHelpMetaList do
  begin
    fMetaIdentifier := fChildHelpMeta.identifier;
    self.findOrCreateReader(fMetaIdentifier);
  end;

  self.GetHints(ARequest, fChildrenHelpMetaList, completion);
end;

procedure TOPPHelpHintServer.GenerateMap(ARequest: TOPPHelpHintMappingSaveRequest; completion: TOPPHelpMapGenerationCompletion);
var
  fList: TList<TOPPHelpMeta>;
  fMeta: TOPPHelpMeta;
  fMap: TOPPHelpHintMap;
  fMapList: TList<TOPPHelpHintMap>;
  fFactory: IOPPHelpMetaFactory;
begin

  if not Assigned(ARequest.OnGetHintFactory) then
  begin
    eventLogger.Log('GenerateMap - OnGetHintFactory is not defined', lmError);
    completion(nil);
    exit;
  end;

  fFactory := ARequest.OnGetHintFactory();

  fMapList := TList<TOPPHelpHintMap>.Create();
  try
    fList := fFactory.GetChildrenHelpMeta(ARequest.Control);
    try

      for fMeta in fList do
      begin
        fMap := TOPPHelpHintMap.Create(fMeta.identifier, TOPPHelpPredicate.Create);
        if Length(ARequest.DefaultPredicateFileName) > 0 then
          fMap.Predicate.filename := ARequest.DefaultPredicateFileName;
        fMapList.Add(fMap);
      end;

      helpHintServer.MergeMaps(fMapList);
      helpHintServer.SaveMaps(ARequest.MappingFileName);

      if Assigned(completion) then
      begin
        completion(fMapList);
      end;
    finally
      fList.Free;
    end;
  finally
    fMapList.Free;
  end;
end;

procedure TOPPHelpHintServer.SaveMaps(AFileName: String);
begin
  TOPPHelpHintMap.saveJSON(fHintMapSet.list, AFileName);
end;

procedure TOPPHelpHintServer.MergeMaps(AList: TList<TOPPHelpHintMap>);
begin
  fHintMapSet.MergeMaps(AList);
end;

{ private }

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
