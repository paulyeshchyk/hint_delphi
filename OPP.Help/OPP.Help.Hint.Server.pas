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
  OPP.Help.System.Str,
  OPP.Help.System.References,
  OPP.Help.Predicate,
  OPP.Help.Map,
  OPP.Help.Interfaces,
  OPP.Help.Meta,

  Datasnap.DBClient, Data.DB;

type
  TOPPHelpHintLoadCompletion = reference to procedure(HintTexts: TList<TOPPHelpHint>);
  TOPPHelpMapGenerationCompletion = reference to procedure(AList: TList<TOPPHelpMap>);
  TOPPHelpHintServerOnGetMetaFactory = reference to function(AComponent: TComponent): TList<TOPPHelpMeta>;
  TOPPHelpHintViewCreator = reference to function(AHintMap: TOPPHelpMap): IOPPHelpHintDataReader;
  TOPPHelpMapsCompletion = reference to procedure(AList: TList<TOPPHelpMap>);

  TOPPHelpHintMappingRequest = class
  private
    fMappingFileName: String;
    fControl: TControl;
    fOnGetHintFactory: TOPPHelpHintServerOnGetMetaFactory;
  public
    constructor Create(AControl: TControl; AMappingFileName: String = '');
    destructor Destroy; override;
    property MappingFileName: String read fMappingFileName;
    property Control: TControl read fControl;
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

  IOPPHelpMapContainer = interface
    procedure RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion);
    procedure CreateHelpMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);
    function AddHelpMap(AMap: TOPPHelpMap): Boolean;
    function GetAvailableMaps(): TList<TOPPHelpMap>;
    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
    procedure AvailableMaps(completion: TOPPHelpMapsCompletion);
    procedure MergeHelpMaps(AList: TList<TOPPHelpMap>);
    procedure SaveHelpMaps(AFileName: String; callback: TOPPHelpErrorCompletion); overload;
    procedure SaveHelpMaps(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion); overload;
    procedure ValidateHelpMapIdentifier(AIdentificator, ANewIdentifier: String; completion: TOPPHelpBooleanCompletion);
  end;

  IOPPHelpHintServer = interface(IOPPHelpMapContainer)

    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;
    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);

    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);
  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fDefaultOnHintReaderCreator: TOPPHelpHintViewCreator;
    fHintDataReaders: TDictionary<String, IOPPHelpHintDataReader>;
    fHintMapSet: TOPPHelpMapSet;
    fHintMetaDict: TDictionary<TSymbolName, String>;
    fLoaded: Boolean;
    function findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
    function GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
    procedure GetHints(ARequest: TOPPHelpHintMappingLoadRequest; hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;
    function getReader(AFileName: String): IOPPHelpHintDataReader;
    procedure reloadConfigurationIfNeed(filename: String);
  public
    constructor Create;
    destructor Destroy; override;
    function AddHelpMap(AMap: TOPPHelpMap): Boolean;
    procedure AvailableMaps(completion: TOPPHelpMapsCompletion);
    function GetAvailableMaps: TList<TOPPHelpMap>;
    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
    procedure CreateHelpMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);
    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;
    procedure MergeHelpMaps(AList: TList<TOPPHelpMap>);
    procedure RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion);
    procedure SaveHelpMaps(AFileName: String; callback: TOPPHelpErrorCompletion); overload;
    procedure SaveHelpMaps(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion); overload;
    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);
    procedure ValidateHelpMapIdentifier(AIdentificator, ANewIdentifier: String; completion: TOPPHelpBooleanCompletion);
    property IsLoaded: Boolean read fLoaded;
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

uses
  OPP.Help.Map.Parser.JSON,
  OPP.Help.System.Files,
  OPP.Help.System.Error,
  OPP.Help.Log;

const
  kContext = 'OPPHelpHintServer';
  kHintsMappingDefaultFileName: String = '.\Документация\hint.idx';

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

constructor TOPPHelpHintMappingRequest.Create(AControl: TControl; AMappingFileName: String);
begin
  inherited Create;

  fControl := AControl;

  if Length(AMappingFileName) <> 0 then
    fMappingFileName := AMappingFileName
  else
    fMappingFileName := kHintsMappingDefaultFileName;
end;

destructor TOPPHelpHintMappingRequest.Destroy;
begin
  fControl := nil;
  inherited;
end;

constructor TOPPHelpHintServer.Create;
begin
  fHintDataReaders := TDictionary<String, IOPPHelpHintDataReader>.Create();
  fHintMapSet := TOPPHelpMapSet.Create();
  fHintMetaDict := TDictionary<TSymbolName, String>.Create();
end;

destructor TOPPHelpHintServer.Destroy;
begin

  fHintDataReaders.Free;
  fHintMapSet.Free;
  fHintMetaDict.Free;

  inherited Destroy;
end;

function TOPPHelpHintServer.AddHelpMap(AMap: TOPPHelpMap): Boolean;
var
  fIndex: Integer;
begin
  result := false;
  if not Assigned(AMap) then
  begin
    eventLogger.Error('HintMap is not defined');
    exit;
  end;

  fIndex := fHintMapSet.list.Add(AMap);
  result := (fIndex >= 0);
end;

procedure TOPPHelpHintServer.AvailableMaps(completion: TOPPHelpMapsCompletion);
begin
  if not Assigned(completion) then
    exit;
  completion(fHintMapSet.list);
end;

procedure TOPPHelpHintServer.CreateHelpMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);
var
  fHelpMap: TOPPHelpMap;
  fID: String;
begin
  if not Assigned(completion) then
  begin
    eventLogger.Error('NewMap completion was not defined');
    exit;
  end;

  fID := GUIDToString(newGUID);
  eventLogger.Flow(Format('Created hint map: %s', [fID]), kContext);
  fHelpMap := TOPPHelpMap.Create(fID);
  try

    if Assigned(onApplyDefaults) then
      onApplyDefaults(@fHelpMap);

    fHintMapSet.AddMap(fHelpMap);
    completion(fHelpMap);
  finally
    // fHelpMap.Free;
  end;
end;

procedure TOPPHelpHintServer.FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
var
  fMap: TOPPHelpMap;
  result: TOPPHelpMap;
begin
  result := nil;
  if not Assigned(completion) then
  begin
    eventLogger.Error('FindMap completion is not defined');
    exit;
  end;

  for fMap in fHintMapSet.list do
  begin
    if fMap = nil then
      continue;
    if not Assigned(fMap) then
      continue;
    if fMap.ComponentIdentifier = AIdentifier then
    begin
      result := fMap;
      break;
    end;
  end;

  completion(result);
end;

{ public }

// TODO: add callback where hintmap and reader be returned
// reader will take predicate from hintmap and then it should run search using predicate
function TOPPHelpHintServer.findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
var
  fMap: TOPPHelpMap;
  fPredicate: TOPPHelpPredicate;
begin
  result := nil;

  if fHintMapSet.list.Count = 0 then
  begin
    eventLogger.Warning(Format('maps list is empty; skipping search for [%s]', [AMetaIdentifier]));
    exit;
  end;

  if Length(AMetaIdentifier) = 0 then
  begin
    eventLogger.Warning(Format('MetaIdentifier is empty', [AMetaIdentifier]));
    exit;
  end;

  fMap := fHintMapSet.GetMap(AMetaIdentifier);
  if not Assigned(fMap) then
  begin
    // eventLogger.Debug(Format('map was not found for identifier [%s]', [AMetaIdentifier]));
    exit;
  end;

  eventLogger.Debug(Format('map was found for %s', [AMetaIdentifier]));

  fPredicate := fMap.Predicate;
  if not Assigned(fPredicate) then
  begin
    eventLogger.Error(Format('predicate was not found for %s', [AMetaIdentifier]));
    exit;
  end;

  result := getReader(fMap.Predicate.filename);
  if Assigned(result) then
  begin
    eventLogger.Debug(Format('reader was found for:[%s]', [fMap.Predicate.filename]));
    exit;
  end;

  if not Assigned(fDefaultOnHintReaderCreator) then
  begin
    eventLogger.Debug(Format('hintreader creator was not defined for:[%s]', [fMap.Predicate.filename]));
    exit;
  end;

  result := fDefaultOnHintReaderCreator(fMap);
  if not Assigned(result) then
  begin
    eventLogger.Debug(Format('hintreader was not created for:[%s]', [fMap.Predicate.filename]));
    exit;
  end;

  fHintDataReaders.Add(fMap.Predicate.filename, result);
end;

function TOPPHelpHintServer.GetAvailableMaps: TList<TOPPHelpMap>;
begin
  result := fHintMapSet.list;
end;

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
begin
  result.Data := GetHintData(hintMeta.Identifier);
  result.Meta := hintMeta;
end;

function TOPPHelpHintServer.GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
var
  fHintMap: TOPPHelpMap;
  fReader: IOPPHelpHintDataReader;
begin

  fReader := findOrCreateReader(AHintIdentifier);
  if not Assigned(fReader) then
    exit;

  fHintMap := fHintMapSet.GetMap(AHintIdentifier);
  if not Assigned(fHintMap) then
    exit;

  result := fReader.FindHintDataForBookmarkIdentifier(fHintMap.Predicate);
end;

procedure TOPPHelpHintServer.GetHints(ARequest: TOPPHelpHintMappingLoadRequest; hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion);
var
  fHint: TOPPHelpHint;
  fHintMeta: TOPPHelpMeta;
  fHintTexts: TList<TOPPHelpHint>;
begin

  self.reloadConfigurationIfNeed(ARequest.MappingFileName);
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fHintTexts := TList<TOPPHelpHint>.Create;
  try
    //TODO: PY Performance
    for fHintMeta in hintsMetaList do
    begin
      fHint := GetHint(fHintMeta);
      if not fHint.Data.isEmpty() then
      begin
        fHintTexts.Add(fHint);
      end;
    end;
    completion(fHintTexts);
  finally
    fHintTexts.Free;
  end;
end;

function TOPPHelpHintServer.getReader(AFileName: String): IOPPHelpHintDataReader;
begin

  result := nil;

  if fHintDataReaders.Count = 0 then
  begin
    eventLogger.Debug(Format('Found no reader for file: %s', [AFileName]));
    exit;
  end;

  try
    try
      fHintDataReaders.TryGetValue(AFileName, result);
    except
      on e: Exception do
      begin
        eventLogger.Error(e);
      end;
    end;

  finally

  end;
end;

procedure TOPPHelpHintServer.LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion);
var
  fChildHelpMeta: TOPPHelpMeta;
  fChildrenHelpMetaList: TList<TOPPHelpMeta>;
  fMetaIdentifier: TOPPHelpHintMapIdentifier;
begin

  eventLogger.Flow(Format('Load hints started for [%s]', [ARequest.Control.ClassName]), kContext);

  self.reloadConfigurationIfNeed(ARequest.MappingFileName);
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  if not Assigned(ARequest.OnGetHintFactory) then
  begin
    eventLogger.Error('GetHints - OnGetHintFactory is not defined');
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fChildrenHelpMetaList := ARequest.OnGetHintFactory(ARequest.Control);
  if (not Assigned(fChildrenHelpMetaList)) then
  begin
    eventLogger.Debug(Format('Class [%s] has no controls, that are suporting hints', [ARequest.Control.ClassName]));
    completion(nil);
    exit;
  end;

  try

    //TODO: PY Performance
    for fChildHelpMeta in fChildrenHelpMetaList do
    begin
      fMetaIdentifier := fChildHelpMeta.Identifier;
      self.findOrCreateReader(fMetaIdentifier);
    end;

    eventLogger.Flow(Format('Will create hints for [%s]', [ARequest.Control.ClassName]), kContext);

    self.GetHints(ARequest, fChildrenHelpMetaList, completion);
  finally
    fChildrenHelpMetaList.Free;
  end;
end;

procedure TOPPHelpHintServer.MergeHelpMaps(AList: TList<TOPPHelpMap>);
begin
  fHintMapSet.MergeMaps(AList);
end;

{ private }

procedure TOPPHelpHintServer.reloadConfigurationIfNeed(filename: String);
var
  fOPPHelpHintMapJSONReadCallback: TOPPHelpMapParserJSONCallback;
begin

  if fLoaded then
  begin
    exit;
  end;

  fOPPHelpHintMapJSONReadCallback := procedure(Mapset: TOPPHelpMapSet; Error: Exception)
    var
      fMap: TOPPHelpMap;

    begin

      if Assigned(Error) then
      begin
        eventLogger.Error(Error);
        exit;
      end;

      if not Assigned(Mapset) then
      begin
        eventLogger.Error('Mapset is not defined');
        exit;
      end;

      if Assigned(Mapset.list) then
      begin
        eventLogger.Flow(Format('Load hints finished; added [%d] maps', [Mapset.list.Count]), kContext);

        for fMap in Mapset.list do
        begin
          fHintMapSet.AddMap(fMap);
        end;
      end;

      self.fLoaded := true;

    end;

  TOPPHelpMapRESTParser.readJSON(filename, fOPPHelpHintMapJSONReadCallback);

end;

procedure TOPPHelpHintServer.RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion);
var
  fMap: TOPPHelpMap;
  itemsToRemove: TList<TOPPHelpMap>;
begin

  itemsToRemove := TList<TOPPHelpMap>.Create();
  try
    for fMap in self.fHintMapSet.list do
    begin
      if fMap = nil then
        continue;
      if fMap.ComponentIdentifier = AIdentifier then
        itemsToRemove.Add(fMap);
    end;

    for fMap in itemsToRemove do
    begin
      if fMap = nil then
        continue;
      eventLogger.Flow(Format('Removed record: [%s]', [fMap.Identifier]), kContext);
      fHintMapSet.list.Remove(fMap);
    end;

  finally
    itemsToRemove.Free;
    self.SaveHelpMaps('', callback);
  end;

end;

procedure TOPPHelpHintServer.SaveHelpMaps(AFileName: String; callback: TOPPHelpErrorCompletion);
var
  fFileName: String;
  fFileNameFullPath: String;
begin
  if Length(AFileName) = 0 then
    fFileName := kHintsMappingDefaultFileName
  else
    fFileName := AFileName;

  eventLogger.Flow(Format('Did saved maps in %s', [fFileName]), kContext);

  fFileNameFullPath := TOPPHelpSystemFilesHelper.AbsolutePath(fFileName);

  SaveHelpMaps(fHintMapSet.list, fFileNameFullPath, callback);
end;

procedure TOPPHelpHintServer.SaveHelpMaps(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion);
begin
  AList.Pack;
  TOPPHelpMapRESTParser.saveJSON(AList, AFileName, callback);
end;

procedure TOPPHelpHintServer.SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
var
  fList: TList<TOPPHelpMeta>;
  fListOfUniques: TList<String>;
  fMap: TOPPHelpMap;
  fMapList: TList<TOPPHelpMap>;
  fMeta: TOPPHelpMeta;
begin

  if not Assigned(ARequest.OnGetHintFactory) then
  begin
    eventLogger.Error('GenerateMap - OnGetHintFactory is not defined');
    completion(nil);
    exit;
  end;

  fListOfUniques := TList<String>.Create();
  fMapList := TList<TOPPHelpMap>.Create();
  try
    fList := ARequest.OnGetHintFactory(ARequest.Control);
    try
      for fMeta in fList do
      begin
        if fListOfUniques.Contains(fMeta.Identifier) then
          continue;
        fListOfUniques.Add(fMeta.Identifier);

        fMap := TOPPHelpMap.Create(fMeta.Identifier);

        if Length(ARequest.DefaultPredicateFileName) > 0 then
          fMap.Predicate.filename := ARequest.DefaultPredicateFileName;
        fMapList.Add(fMap);
      end;

      if useGlobal then
      begin
        helpHintServer.MergeHelpMaps(fMapList);
        helpHintServer.SaveHelpMaps(ARequest.MappingFileName,
          procedure(AError: Exception)
          begin
            if Assigned(completion) then
            begin
              completion(fMapList);
            end;
          end);
      end else begin
        helpHintServer.SaveHelpMaps(fMapList, ARequest.MappingFileName,
          procedure(AError: Exception)
          begin
            if Assigned(completion) then
            begin
              completion(fMapList);
            end;
          end);
      end;

    finally
      fList.Free;
    end;
  finally
    fMapList.Free;
    fListOfUniques.Free;
  end;
end;

procedure TOPPHelpHintServer.setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);
begin
  fDefaultOnHintReaderCreator := ACreator;
end;

procedure TOPPHelpHintServer.ValidateHelpMapIdentifier(AIdentificator, ANewIdentifier: String; completion: TOPPHelpBooleanCompletion);
begin
  if not Assigned(completion) then
    exit;

  if Length(ANewIdentifier) = 0 then
  begin
    completion(false);
    exit;
  end;

  if AIdentificator = ANewIdentifier then
  begin
    completion(true);
    exit;
  end;

  self.FindHelpMap(ANewIdentifier,
    procedure(const AMap: TOPPHelpMap)
    begin
      completion(AMap = nil);
    end);

end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
