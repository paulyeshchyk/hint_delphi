unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs, System.SysUtils, System.Classes, System.Generics.Collections,
  System.TypInfo,
  WinAPI.Windows,
  VCL.Controls,
  //
  OPP.Help.Hint, OPP.Help.Meta.Enumerator,
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

  TOPPHelpHintServerOnHintTextsFilenameRequest = reference to function(): string;

  IOPPHelpHintServer = interface

    /// <summary>
    /// Возвращает список подсказок, применимых для компонента, указанного в параметре Control.
    ///
    /// </summary>
    /// <remarks> </remarks>
    procedure GetHints(Control: TControl; filename: String; completion: TOPPHelpHintLoadCompletion); overload;

    procedure GenerateMap(AControl: TControl; defaultPredicateFileName: String; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);

  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private
    fLoaded: Boolean;
    fHintMapSet: TOPPHelpHintMapSet;

    fHintMetaDict: TDictionary<TSymbolName, String>;

    fHintDataReaders: TDictionary<String, IOPPHelpHintDataReader>;
    procedure reloadConfigurationIfNeed(filename: String);
    function findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
    function getReader(AFileName: String): IOPPHelpHintDataReader;
  public
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    function GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
    procedure GetHints(Control: TControl; filename: String; completion: TOPPHelpHintLoadCompletion); overload;
    procedure GetHints(hintsMetaList: TOPPHintIdList; filename: String; completion: TOPPHelpHintLoadCompletion); overload;

    procedure registerHintMeta(propertyName: String; component: PTypeInfo);

    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint; overload;
    procedure GenerateMap(AControl: TControl; defaultPredicateFileName: String; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);
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

procedure TOPPHelpHintServer.registerHintMeta(propertyName: String; component: PTypeInfo);
begin
  fHintMetaDict.Add(component.Name, propertyName);
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

procedure TOPPHelpHintServer.GetHints(hintsMetaList: TOPPHintIdList; filename: String; completion: TOPPHelpHintLoadCompletion);
var
  fHintMeta: TOPPHelpMeta;
  fHint: TOPPHelpHint;
  result: TList<TOPPHelpHint>;
begin

  self.reloadConfigurationIfNeed(filename);
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

procedure TOPPHelpHintServer.GetHints(Control: TControl; filename: String; completion: TOPPHelpHintLoadCompletion);
var
  fChildrenHelpMetaList: TList<TOPPHelpMeta>;
  fChildHelpMeta: TOPPHelpMeta;
  fMetaIdentifier: TOPPHelpHintMapIdentifier;
begin

  eventLogger.Log('Hintserver.gethints(Control)');

  self.reloadConfigurationIfNeed(filename);
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fChildrenHelpMetaList := Control.GetChildrenHelpMeta();
  for fChildHelpMeta in fChildrenHelpMetaList do
  begin
    fMetaIdentifier := fChildHelpMeta.identifier;
    self.findOrCreateReader(fMetaIdentifier);
  end;

  self.GetHints(fChildrenHelpMetaList, filename, completion);
end;

procedure TOPPHelpHintServer.GenerateMap(AControl: TControl; defaultPredicateFileName: String; completion: TOPPHelpMapGenerationCompletion);
var
  fList: TList<TOPPHelpMeta>;
  fMeta: TOPPHelpMeta;
  fMap: TOPPHelpHintMap;
  fMapList: TList<TOPPHelpHintMap>;
begin

  fMapList := TList<TOPPHelpHintMap>.Create();
  try
    fList := AControl.GetChildrenHelpMeta();
    try

      for fMeta in fList do
      begin
        fMap := TOPPHelpHintMap.Create(fMeta.identifier, TOPPHelpPredicate.Create);
        if Length(defaultPredicateFileName) > 0 then
          fMap.Predicate.filename := defaultPredicateFileName;
        fMapList.Add(fMap);
      end;

      if Assigned(completion) then
        completion(fMapList);
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
