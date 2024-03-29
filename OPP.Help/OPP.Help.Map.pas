unit OPP.Help.Map;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  OPP.Help.System.References,
  OPP.Help.Predicate;

type

  TOPPHelpMapSet = class;
  POPPHelpMap = ^TOPPHelpMap;

  TOPPHelpMap = class(TObject)
  private
    fComponentIdentifier: TOPPHelpHintMapIdentifier;
    fIdentifier: TOPPHelpHintMapIdentifier;
    fPredicate: TOPPHelpPredicate;
    function GetIsValid: Boolean;
    function GetIsRunnable: Boolean;
  public
    constructor Create; overload;
    constructor Create(AIdentifier: String); overload;
    destructor Destroy; override;

    property ComponentIdentifier: TOPPHelpHintMapIdentifier read fComponentIdentifier write fComponentIdentifier;
    property Identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
    property IsValid: Boolean read GetIsValid;
    property IsRunnable: Boolean read GetIsRunnable;
    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
  end;

  TOPPHelpMapList = class(TList<TOPPHelpMap>)
  end;

  TOPPHelpMapApplyDefaultsCompletion = reference to procedure(const AMap: POPPHelpMap);
  TOPPHelpMapCompletion = reference to procedure(const AMap: TOPPHelpMap);
  TOPPHelpMapParserJSONCallback = reference to procedure(Mapset: TOPPHelpMapSet; Error: Exception);

  TOPPHelpMapSet = class(TObject)
  private
    fList: TOPPHelpMapList;
  public
    constructor Create(AList: TOPPHelpMapList = nil);
    destructor Destroy; override;
    procedure AddMap(AMap: TOPPHelpMap);
    procedure AddMaps(AList: TOPPHelpMapList);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
    procedure MergeMaps(AList: TOPPHelpMapList);
    property list: TOPPHelpMapList read fList;
  end;

implementation

constructor TOPPHelpMap.Create(AIdentifier: String);
begin
  inherited Create;

  fIdentifier := AIdentifier;

  fComponentIdentifier := AIdentifier;
  fPredicate := TOPPHelpPredicate.Create;
end;

constructor TOPPHelpMap.Create;
begin
  inherited Create;
  fPredicate := TOPPHelpPredicate.Create;
end;

destructor TOPPHelpMap.Destroy;
begin
  fComponentIdentifier := '';
  fIdentifier := '';
  FreeAndNil(fPredicate);
  inherited;
end;

function TOPPHelpMap.GetIsRunnable: Boolean;
begin
  result := false;
  if Assigned(fPredicate) then
    result := fPredicate.isRunnable;
end;

function TOPPHelpMap.GetIsValid: Boolean;
begin
  result := Length(fComponentIdentifier) <> 0;
end;

constructor TOPPHelpMapSet.Create(AList: TOPPHelpMapList = nil);
begin
  inherited Create;
  fList := TOPPHelpMapList.Create;
  if Assigned(AList) then
  begin
    fList.AddRange(AList);
  end;
end;

destructor TOPPHelpMapSet.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

procedure TOPPHelpMapSet.AddMap(AMap: TOPPHelpMap);
begin
  if not Assigned(AMap) then
  begin
    exit;
  end;

  fList.Add(AMap);
end;

procedure TOPPHelpMapSet.AddMaps(AList: TOPPHelpMapList);
var
  fItem: TOPPHelpMap;
begin
  if not Assigned(AList) then
    exit;

  for fItem in AList do
  begin
    fList.Add(fItem);
  end;

end;

function TOPPHelpMapSet.GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
var
  fHintMap: TOPPHelpMap;
begin
  result := nil;
  for fHintMap in fList do
  begin
    if fHintMap = nil then
      continue;
    if CompareStr(fHintMap.ComponentIdentifier, AHintIdentifier) = 0 then
    begin
      result := fHintMap;
      break;
    end;
  end;
end;

procedure TOPPHelpMapSet.MergeMaps(AList: TOPPHelpMapList);
var
  fItem: TOPPHelpMap;
  fDictionary: TDictionary<String, TOPPHelpMap>;
begin
  if not Assigned(AList) then
    exit;

  fDictionary := TDictionary<String, TOPPHelpMap>.Create;
  try
    for fItem in fList do
    begin
      if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
        fDictionary.Add(fItem.ComponentIdentifier, fItem);
    end;

    for fItem in AList do
    begin
      if fItem.IsValid then
      begin
        if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
          fList.Add(fItem);
      end;
    end;
  finally
    fDictionary.Clear;
    fDictionary.Free;
  end;

  fList.Pack;
end;

end.
