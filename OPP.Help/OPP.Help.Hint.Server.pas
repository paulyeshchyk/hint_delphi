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
  OPP.Help.Meta;

type
  TOPPHelpHintLoadCompletion = reference to procedure(HintTexts: TList<TOPPHelpHint>);
  TOPPHelpMapGenerationCompletion = reference to procedure(AList: TList<TOPPHelpMap>);
  TOPPHelpHintServerOnGetMetaFactory = reference to function(AComponent: TComponent): TList<TOPPHelpMeta>;
  TOPPHelpHintViewCreator = reference to function(AHintMap: TOPPHelpMap): IOPPHelpHintDataReader;

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

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    procedure SaveMaps(AFileName: String);
    procedure SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String);

    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);
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

    procedure LoadHints(const ARequest: TOPPHelpHintMappingLoadRequest; completion: TOPPHelpHintLoadCompletion); overload;

    procedure SaveHints(ARequest: TOPPHelpHintMappingSaveRequest; useGlobal: Boolean; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    procedure SaveMaps(AFileName: String);
    procedure SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String);

    procedure setDefaultOnHintReaderCreator(ACreator: TOPPHelpHintViewCreator);

    property loaded: Boolean read fLoaded;
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

uses
  OPP.Help.Map.Filereader,
  OPP.Help.System.Error,
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

constructor TOPPHelpHintMappingRequest.Create(AControl: TControl; AMappingFileName: String);
begin
  inherited Create;

  fControl := AControl;

  if Length(AMappingFileName) <> 0 then
    fMappingFileName := AMappingFileName
  else
    fMappingFileName := '.\help\mapping\hints_matrix.json';
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

{ private }

procedure TOPPHelpHintServer.reloadConfigurationIfNeed(filename: String);
var
  fOPPHelpHintMapJSONReadCallback: TOPPHelpHintMapJSONReadCallback;
begin

  if Length(filename) = 0 then
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
    eventLogger.Debug(Format('map was not found for identifier [%s]', [AMetaIdentifier]));
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
  result.data := GetHintData(hintMeta.identifier);
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
    if not fHint.data.isEmpty() then
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

        fMap := TOPPHelpMap.Create();
        fMap.identifier := fMeta.identifier;
        fMap.Predicate := TOPPHelpPredicate.Create;
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

procedure TOPPHelpHintServer.SaveMaps(AFileName: String);
begin
  TOPPHelpMap.saveJSON(fHintMapSet.list, AFileName);
end;

procedure TOPPHelpHintServer.SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String);
begin
  TOPPHelpMap.saveJSON(AList, AFileName);
end;

procedure TOPPHelpHintServer.MergeMaps(AList: TList<TOPPHelpMap>);
begin
  fHintMapSet.MergeMaps(AList);
end;

{ private }

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
