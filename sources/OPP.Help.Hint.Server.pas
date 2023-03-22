﻿unit OPP.Help.Hint.Server;

interface

uses
  System.SyncObjs, System.SysUtils, System.Classes, System.Generics.Collections,
  System.TypInfo,
  VCL.Controls,
  //
  OPP.Help.Hint, OPP.Help.Vcl.Control.Hint,
  //
  OPP.Help.Nonatomic,
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

    procedure registerHintMeta(propertyName: String; component: PTypeInfo);

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
    function findOrCreateReader(AHintIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
    function getReader(AFileName: String): IOPPHelpHintDataReader;

    procedure temporarySave();
  public
    property loaded: Boolean read fLoaded;

    constructor Create;
    destructor Destroy; override;

    function GetHintData(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintData;
    procedure GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion); overload;
    procedure GetHints(hintsMetaList: TOPPHintIdList; completion: TOPPHelpHintLoadCompletion); overload;

    procedure registerHintMeta(propertyName: String; component: PTypeInfo);

    function GetHint(hintMeta: TOPPHelpHintMeta): TOPPHelpHint; overload;

    function getOnHintTextsFileNameRequest(): TOPPHelpHintServerOnHintTextsFilenameRequest;
    procedure setOnHintTextsFileNameRequest(value: TOPPHelpHintServerOnHintTextsFilenameRequest);
    property OnGetHintConfigurationFileNameRequest: TOPPHelpHintServerOnHintTextsFilenameRequest read fOnHintTextsFileNameRequest write fOnHintTextsFileNameRequest;
  end;

function helpHintServer: IOPPHelpHintServer;

implementation

uses
  OPP.Help.Hint.Mapping.Filereader, Windows, OPP.Help.System;

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

      self.fLoaded := true;

      if Assigned(error) then
      begin
        error.Log();
        exit;
      end;
      if Assigned(AList) then
        fHintMapSet.list.AddRange(AList);

    end;

  fFileName := fOnHintTextsFileNameRequest();
  TOPPHelpHintMap.readJSON(fFileName, fOPPHelpHintMapJSONReadCallback);

  // temporarySave;

end;

procedure TOPPHelpHintServer.temporarySave();
// var
// list: TList<TOPPHelpHintMap>;
// testValue : TOPPHelpHintMap;
// kw : TOPPHelpKeyword;
begin
  // kw := TOPPHelpKeyword.Create();
  // kw.bookmarkID := 'bookmarkID';
  // kw.searchPattern := 'searchPattern';
  // kw.page := '1';
  // testValue := TOPPHelpHintMap.Create(kw, 'zz.rtf');
  // list := TList<TOPPHelpHintMap>.create;
  // list.Add(testValue);
  //
  // TOPPHelpHintMap.saveJSON(list, 'help\mapping\hints_matrix1.json')
end;

{ public }

//TODO: add callback where hintmap and reader be returned
//reader will take predicate from hintmap and then it should run search using predicate
function TOPPHelpHintServer.findOrCreateReader(AHintIdentifier: TOPPHelpHintMapIdentifier): IOPPHelpHintDataReader;
var
  fMap: TOPPHelpHintMap;
begin
  result := nil;

  fMap := fHintMapSet.GetMap(AHintIdentifier);
  if not Assigned(fMap) then
    exit;

  result := getReader(fMap.predicate.filename);
  if Assigned(result) then
    exit;

  result := TOPPHelpRichtextHintReader.Create;
  result.loadData(fMap.predicate.filename);
  fHintDataReaders.Add(fMap.predicate.filename, result);
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
begin

  self.reloadConfigurationIfNeed();
  if not fLoaded then
    exit;

  fReader := findOrCreateReader(AHintIdentifier);
  if Assigned(fReader) then
  begin
    result := fReader.FindHintDataForBookmarkIdentifier(AHintIdentifier);
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
      result.Add(fHint);
    end;
  end;
  completion(result);
end;

procedure TOPPHelpHintServer.GetHints(Control: TControl; completion: TOPPHelpHintLoadCompletion);
var
  fChildrenInfoList: TList<TOPPHelpHintMeta>;
  fChildInfo: TOPPHelpHintMeta;
  fHintIdentifier: TOPPHelpHintMapIdentifier;
begin

  self.reloadConfigurationIfNeed();
  if not fLoaded then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  fChildrenInfoList := Control.GetChildrenHintsMeta();
  for fChildInfo in fChildrenInfoList do
  begin
    fHintIdentifier := fChildInfo.hintIdentifier;

    self.findOrCreateReader(fHintIdentifier);
  end;

  self.GetHints(fChildrenInfoList, completion);
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
