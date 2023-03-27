unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs, System.SysUtils, System.Classes, System.Generics.Collections,
  System.TypInfo,
  WinAPI.Windows,
  VCL.Controls,
//  //
  OPP.Help.Hint,
  OPP.Help.Meta.Enumerator,
//  //
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
    //
    // /// <summary>
    // /// ¬озвращает подсказку дл€ компонента, метаданные которого указаны в параметре hintMeta.
    // ///
    // /// </summary>
    // /// <remarks> </remarks>
    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint;
    //
    // /// <summary>
    // /// ¬озвращает список подсказок, применимых дл€ компонента, указанного в параметре Control.
    // ///
    // /// </summary>
    // /// <remarks> </remarks>
    procedure GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion); overload;
    //
    // /// <summary>
    // /// ¬озвращает список подсказок, применимых дл€ списка идентификаторов, вз€тых из компонента.
    // ///
    // /// </summary>
    // /// <remarks> </remarks>
    procedure GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;
    function getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);

    procedure registerHintMeta(propertyName: String; component: PTypeInfo);

    procedure GenerateMap(AControl: TControl; defaultPredicateFileName: String; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);

    property OnGetHintConfigurationFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest read getOnHintTextsFileNameRequest write setOnHintTextsFileNameRequest;
  end;

  TOPPHelpHintServer = class(TInterfacedObject, IOPPHelpHintServer)
  private

    fLoaded: Boolean;
    fHintMapSet: TOPPHelpHintMapSet;

    fHintMetaDict: TDictionary<TSymbolName, String>;

    fHintDataReaders: TDictionary<String, IOPPHelpHintDataReader>;
    fOnHintTextsFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure reloadConfigurationIfNeed();
    function findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
    function getReader(AFileName: String): IOPPHelpHintDataReader;
  public

    class function sharedInstance(): IOPPHelpHintServer;

    constructor Create;
    destructor Destroy; override;

    function GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
    procedure GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion); overload;
    procedure GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;

    procedure registerHintMeta(propertyName: String; component: PTypeInfo);

    function GetHint(hintMeta: TOPPHelpMeta): TOPPHelpHint; overload;
    procedure GenerateMap(AControl: TControl; defaultPredicateFileName: String; completion: TOPPHelpMapGenerationCompletion);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    procedure SaveMaps(AFileName: String);

    function getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
    property OnGetHintConfigurationFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest read fOnHintTextsFileNameRequest write fOnHintTextsFileNameRequest;
    property loaded: Boolean read fLoaded;

  end;

//function helpHintServer: IOPPHelpHintServer;


implementation

uses
  OPP.Help.Hint.Mapping.Filereader, OPP.Help.System.Error;

var
  fOPPHelpHintServerLock: TCriticalSection;
  fHelpHintServer: IOPPHelpHintServer;


class function TOPPHelpHintServer.sharedInstance(): IOPPHelpHintServer;
begin
  fOPPHelpHintServerLock.Acquire;
  try
    if not Assigned(fHelpHintServer) then
    begin
      fHelpHintServer := TOPPHelpHintServer.Create;
    end;
    result := fHelpHintServer;
  finally
    fOPPHelpHintServerLock.Release;
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

procedure TOPPHelpHintServer.reloadConfigurationIfNeed();
var
  fFileName: string;
  fOPPHelpHintMapJSONReadCallback: TOPPHelpHintMapJSONReadCallback;
begin
  if fLoaded then
    exit;

  if not Assigned(fOnHintTextsFileNameRequest) then
    exit;

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

  fFileName := fOnHintTextsFileNameRequest();
  TOPPHelpHintMap.readJSON(fFileName, fOPPHelpHintMapJSONReadCallback);

end;


// TODO: add callback where hintmap and reader be returned
// reader will take predicate from hintmap and then it should run search using predicate
function TOPPHelpHintServer.findOrCreateReader(AMetaIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
//var
//  fMap: TOPPHelpHintMap;
//  fPredicate: TOPPHelpPredicate;
begin
  result := nil;

//  fMap := fHintMapSet.GetMap(AMetaIdentifier);
//  if not Assigned(fMap) then
//    exit;
//  fPredicate := fMap.Predicate;
//  if not Assigned(fPredicate) then
//  begin
//    WinAPI.Windows.OutputDebugString('!!! PREDICATE NOT FOUND!!!'.toWidechar);
//    exit;
//  end;
//
//  result := getReader(fMap.Predicate.fileName);
//  if Assigned(result) then
//    exit;
//
//  result := TOPPHelpRichtextHintReader.Create;
//  result.loadData(fMap.Predicate.fileName);
//  fHintDataReaders.Add(fMap.Predicate.fileName, result);
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

  self.reloadConfigurationIfNeed();
  if not fLoaded then
    exit;

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

procedure TOPPHelpHintServer.GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion);
var
  fHintMeta: TOPPHelpMeta;
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
      result.Add(fHint);
    end;
  end;
  completion(result);
end;

procedure TOPPHelpHintServer.GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion);
var
  fChildrenHelpMetaList: TList<TOPPHelpMeta>;
  fChildHelpMeta: TOPPHelpMeta;
  fMetaIdentifier: TOPPHelpHintMapIdentifier;
begin

  self.reloadConfigurationIfNeed();
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

  self.GetHints(fChildrenHelpMetaList, completion);
end;

function TOPPHelpHintServer.getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
begin
  result := fOnHintTextsFileNameRequest;
end;

procedure TOPPHelpHintServer.setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
begin
  fOnHintTextsFileNameRequest := value;
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
          fMap.Predicate.fileName := defaultPredicateFileName;
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


initialization

fOPPHelpHintServerLock := TCriticalSection.Create;

finalization

fOPPHelpHintServerLock.Free;

end.
