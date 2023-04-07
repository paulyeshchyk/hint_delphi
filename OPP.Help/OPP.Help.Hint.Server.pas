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

  IOPPHelpHintServer = interface

    procedure NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    ///
    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    function SaveMaps(AFileName: String): Integer;
    function SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String): Integer;

    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);

    procedure makeRecordsDataset(AClientDataSet: TClientDataset);
    procedure loadRecordsDataset(AClientDataSet: TClientDataset);

    procedure makePredicatesDataset(AClientDataSet: TClientDataset);
    procedure loadPredicatesDataset(AClientDataSet: TClientDataset; ARecordId: String);

    function addHintMap(AMap: TOPPHelpMap): Boolean;
    procedure FindMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);

    function availableIdentifiers: TList<TOPPHelpMetaIdentifierType>;
    function removeHint(AIdentifier: TOPPHelpMetaIdentifierType): Integer;
    procedure AvailableMaps(completion: TOPPHelpMapsCompletion);

  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fLoaded: Boolean;
    fHintMapSet: TOPPHelpMapSet;

    fHintMetaDict: TDictionary<TSymbolName, String>;
    fDefaultOnHintReaderCreator: TOPPHelpHintViewCreator;

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

    procedure NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);

    function availableIdentifiers: TList<TOPPHelpMetaIdentifierType>;
    function removeHint(AIdentifier: TOPPHelpMetaIdentifierType): Integer;
    procedure FindMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);

    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    function SaveMaps(AFileName: String = ''): Integer;
    function SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String): Integer;

    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);

    procedure makeRecordsDataset(AClientDataSet: TClientDataset);
    procedure loadRecordsDataset(AClientDataSet: TClientDataset);

    procedure makePredicatesDataset(AClientDataSet: TClientDataset);
    procedure loadPredicatesDataset(AClientDataSet: TClientDataset; ARecordId: String);

    function addHintMap(AMap: TOPPHelpMap): Boolean;

    property loaded: Boolean read fLoaded;
    procedure AvailableMaps(completion: TOPPHelpMapsCompletion);
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

uses
  OPP.Help.System.Files,
  OPP.Help.Map.Filereader,
  OPP.Help.System.Error,
  OPP.Help.Log;

const
  kHintsMappingDefaultFileName: String = '.\help\mapping\hints_matrix.json';

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

constructor TOPPHelpHintServer.Create;
begin
  fHintDataReaders := TDictionary<String, IOPPHelpHintDataReader>.Create();
  fHintMapSet := TOPPHelpMapSet.Create();
  fHintMetaDict := TDictionary<TSymbolName, String>.Create();
end;

destructor TOPPHelpHintServer.Destroy;
begin
  fHintDataReaders := nil;
  inherited Destroy;
end;

procedure TOPPHelpHintServer.AvailableMaps(completion: TOPPHelpMapsCompletion);
begin
  if not Assigned(completion) then
    exit;
  completion(fHintMapSet.list);
end;

procedure TOPPHelpHintServer.NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);
var
  fHelpMap: TOPPHelpMap;
  fID:String;
begin
  if not Assigned(completion) then
  begin
    eventLogger.Error('NewMap completion was not defined');
    exit;
  end;

  fID := GUIDToString(newGUID);
  eventLogger.Flow(Format('Created hint map: %s',[fID]));
  fHelpMap := TOPPHelpMap.Create(fID);
  try
    fHintMapSet.AddMap(fHelpMap);
    completion(fHelpMap);
  finally
    // fHelpMap.Free;
  end;
end;

function TOPPHelpHintServer.removeHint(AIdentifier: TOPPHelpMetaIdentifierType): Integer;
var
  itemsToRemove: TList<TOPPHelpMap>;
  fMap: TOPPHelpMap;
begin

  result := -1;

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
      fHintMapSet.list.Remove(fMap);
    end;

  finally
    itemsToRemove.Free;
    result := self.SaveMaps();
  end;

end;

procedure TOPPHelpHintServer.FindMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
var
  result, fMap: TOPPHelpMap;
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

function TOPPHelpHintServer.availableIdentifiers: TList<TOPPHelpMetaIdentifierType>;
var
  fMeta: TOPPHelpMap;
begin
  reloadConfigurationIfNeed(kHintsMappingDefaultFileName);

  result := TList<TOPPHelpMetaIdentifierType>.Create;
  for fMeta in fHintMapSet.list do
  begin
    if fMeta <> nil then
      result.Add(fMeta.ComponentIdentifier);
  end;
end;

{ private }

procedure TOPPHelpHintServer.reloadConfigurationIfNeed(filename: String);
var
  fOPPHelpHintMapJSONReadCallback: TOPPHelpHintMapJSONReadCallback;
begin

  if not FileExists(filename) then
  begin
    eventLogger.Error('configs filename is not defined');
    exit;
  end;

  eventLogger.Debug(Format('start load config from %s', [filename]));
  if fLoaded then
  begin
    exit;
  end;

  fOPPHelpHintMapJSONReadCallback := procedure(AList: TList<TOPPHelpMap>; Error: Exception)
    begin

      self.fLoaded := true;

      if Assigned(Error) then
      begin
        Error.Log();
        exit;
      end;
      eventLogger.Debug(Format('finish load config; added [%d] maps', [AList.Count]));
      fHintMapSet.AddMaps(AList);
    end;

  TOPPHelpMap.readJSON(filename, fOPPHelpHintMapJSONReadCallback);

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

procedure TOPPHelpHintServer.setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);
begin
  fDefaultOnHintReaderCreator := ACreator;
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
        e.Log();
      end;
    end;

  finally

  end;
end;

function TOPPHelpHintServer.GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
var
  fReader: IOPPHelpHintDataReader;
  fHintMap: TOPPHelpMap;
begin

  fReader := findOrCreateReader(AHintIdentifier);
  if not Assigned(fReader) then
    exit;

  fHintMap := fHintMapSet.GetMap(AHintIdentifier);
  if not Assigned(fHintMap) then
    exit;

  result := fReader.FindHintDataForBookmarkIdentifier(fHintMap.Predicate);
end;

function TOPPHelpHintServer.GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
begin
  result.Data := GetHintData(hintMeta.identifier);
  result.Meta := hintMeta;
end;

procedure TOPPHelpHintServer.GetHints(ARequest: TOPPHelpHintMappingLoadRequest; hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion);
var
  fHintMeta: TOPPHelpMeta;
  fHint: TOPPHelpHint;
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
  for fHintMeta in hintsMetaList do
  begin
    fHint := GetHint(fHintMeta);
    if not fHint.Data.isEmpty() then
    begin
      fHintTexts.Add(fHint);
    end;
  end;
  completion(fHintTexts);
end;

procedure TOPPHelpHintServer.LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion);
var
  fChildrenHelpMetaList: TList<TOPPHelpMeta>;
  fChildHelpMeta: TOPPHelpMeta;
  fMetaIdentifier: TOPPHelpHintMapIdentifier;
  fFactory: IOPPHelpMetaFactory;
begin

  eventLogger.Debug(Format('Loading hints for [%s]', [ARequest.Control.ClassName]));

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
    exit;
  end;

  fChildrenHelpMetaList := ARequest.OnGetHintFactory(ARequest.Control);
  if (not Assigned(fChildrenHelpMetaList)) or (fChildrenHelpMetaList.Count = 0) then
  begin
    eventLogger.Debug(Format('Class [%s] has no controls, that are suporting hints', [ARequest.Control.ClassName]));
    completion(nil);
    exit;
  end;

  for fChildHelpMeta in fChildrenHelpMetaList do
  begin
    fMetaIdentifier := fChildHelpMeta.identifier;
    self.findOrCreateReader(fMetaIdentifier);
  end;

  self.GetHints(ARequest, fChildrenHelpMetaList, completion);
end;

procedure TOPPHelpHintServer.SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
var
  fList: TList<TOPPHelpMeta>;
  fMeta: TOPPHelpMeta;
  fMap: TOPPHelpMap;
  fMapList: TList<TOPPHelpMap>;
  fListOfUniques: TList<String>;
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
        if fListOfUniques.Contains(fMeta.identifier) then
          continue;
        fListOfUniques.Add(fMeta.identifier);

        fMap := TOPPHelpMap.Create(fMeta.identifier);

        if Length(ARequest.DefaultPredicateFileName) > 0 then
          fMap.Predicate.filename := ARequest.DefaultPredicateFileName;
        fMapList.Add(fMap);
      end;

      if useGlobal then
      begin
        helpHintServer.MergeMaps(fMapList);
        helpHintServer.SaveMaps(ARequest.MappingFileName);
      end else begin

        helpHintServer.SaveCustomList(fMapList, ARequest.MappingFileName);
      end;

      if Assigned(completion) then
      begin
        completion(fMapList);
      end;
    finally
      fList.Free;
    end;
  finally
    fMapList.Free;
    fListOfUniques.Free;
  end;
end;

function TOPPHelpHintServer.SaveMaps(AFileName: String = ''): Integer;
var
  fFileName: String;
  fFileNameFullPath: String;
begin
  if Length(AFileName) = 0 then
    fFileName := kHintsMappingDefaultFileName
  else
    fFileName := AFileName;

  fFileNameFullPath := TOPPHelpSystemFilesHelper.AbsolutePath(fFileName);

  result := SaveCustomList(fHintMapSet.list, fFileNameFullPath);
end;

function TOPPHelpHintServer.SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String): Integer;
begin
  result := TOPPHelpMap.saveJSON(AList, AFileName);
end;

procedure TOPPHelpHintServer.MergeMaps(AList: TList<TOPPHelpMap>);
begin
  fHintMapSet.MergeMaps(AList);
end;

procedure TOPPHelpHintServer.makeRecordsDataset(AClientDataSet: TClientDataset);
begin
  if not Assigned(AClientDataSet) then
  begin
    eventLogger.Error('Dataset is not assigned');
    exit;
  end;

  AClientDataSet.Close;
  AClientDataSet.FieldDefs.Add('Identifier', ftString, 255, true);
  AClientDataSet.CreateDataSet;
end;

procedure TOPPHelpHintServer.loadRecordsDataset(AClientDataSet: TClientDataset);
var
  Hint: TOPPHelpMap;
begin
  if not Assigned(AClientDataSet) then
  begin
    eventLogger.Error('Dataset is not assigned');
    exit;
  end;
  if not AClientDataSet.Active then
  begin
    eventLogger.Error('Dataset is not Active');
    exit;
  end;

  for Hint in fHintMapSet.list do
  begin
    AClientDataSet.Insert;
    AClientDataSet.Fields.FieldByName('identifier').asString := Hint.ComponentIdentifier;
    AClientDataSet.Post;
  end;
end;

procedure TOPPHelpHintServer.makePredicatesDataset(AClientDataSet: TClientDataset);
begin
  if not Assigned(AClientDataSet) then
  begin
    eventLogger.Error('Dataset is not assigned');
    exit;
  end;

  AClientDataSet.Close;
  AClientDataSet.FieldDefs.Add('Identifier', ftString, 255, true);
  AClientDataSet.FieldDefs.Add('Predicate', ftString, 255, false);
  AClientDataSet.FieldDefs.Add('KeywordType', ftInteger, 0, false);
  AClientDataSet.CreateDataSet;
end;

procedure TOPPHelpHintServer.loadPredicatesDataset(AClientDataSet: TClientDataset; ARecordId: String);
var
  fMap: TOPPHelpMap;
begin
  if not Assigned(AClientDataSet) then
  begin
    eventLogger.Error('Dataset is not assigned');
    exit;
  end;
  if not AClientDataSet.Active then
  begin
    eventLogger.Error('Dataset is not Active');
    exit;
  end;
  fMap := fHintMapSet.GetMap(ARecordId);
  if not Assigned(fMap) then
    exit;

  AClientDataSet.EmptyDataSet;
  AClientDataSet.Insert;
  AClientDataSet.Fields[0].asString := fMap.ComponentIdentifier;
  AClientDataSet.Fields[1].asString := fMap.Predicate.value;
  AClientDataSet.Fields[2].AsInteger := Integer(fMap.Predicate.keywordType);
  AClientDataSet.Post;
end;

function TOPPHelpHintServer.addHintMap(AMap: TOPPHelpMap): Boolean;
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
{ private }

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
