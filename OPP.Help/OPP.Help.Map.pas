unit OPP.Help.Map;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic,
  OPP.Help.Predicate;

type

  POPPHelpMap = ^TOPPHelpMap;

  TOPPHelpMap = class(TObject)
  private
    fComponentIdentifier: TOPPHelpHintMapIdentifier;
    fIdentifier: TOPPHelpHintMapIdentifier;
    fPredicate: TOPPHelpPredicate;
    function GetIsValid: Boolean;
  public
    constructor Create(AIdentifier: String); overload;
    property ComponentIdentifier: TOPPHelpHintMapIdentifier read fComponentIdentifier write fComponentIdentifier;
    property Identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
    property IsValid: Boolean read GetIsValid;
    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
  end;

  TOPPHelpMapCompletion = reference to procedure(const AMap: TOPPHelpMap);

  TOPPHelpMapSet = class(TObject)
  private
    fList: TList<TOPPHelpMap>;
    function GetList(): TList<TOPPHelpMap>;
  public
    constructor Create(AList: TList<TOPPHelpMap> = nil);
    procedure AddMap(AMap: TOPPHelpMap);
    procedure AddMaps(AList: TList<TOPPHelpMap>);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    property list: TList<TOPPHelpMap> read GetList;
  end;

implementation

constructor TOPPHelpMap.Create(AIdentifier: String);
begin
  inherited Create;

  fIdentifier := AIdentifier;

  fComponentIdentifier := AIdentifier;
  fPredicate := TOPPHelpPredicate.Create;
end;

function TOPPHelpMap.GetIsValid: Boolean;
begin
  result := Length(fComponentIdentifier) <> 0;
end;

constructor TOPPHelpMapSet.Create(AList: TList<OPP.Help.Map.TOPPHelpMap> = nil);
begin
  fList := TList<TOPPHelpMap>.Create;
  if assigned(AList) then
  begin
    fList.AddRange(AList);
  end;
end;

procedure TOPPHelpMapSet.AddMap(AMap: TOPPHelpMap);
begin
  if not assigned(AMap) then
  begin
    exit;
  end;

  fList.Add(AMap);
end;

procedure TOPPHelpMapSet.AddMaps(AList: TList<TOPPHelpMap>);
var
  fItem: TOPPHelpMap;
begin
  if not assigned(AList) then
    exit;

  for fItem in AList do
  begin
    fList.Add(fItem);
  end;

end;

function TOPPHelpMapSet.GetList: TList<TOPPHelpMap>;
begin
  result := fList;
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
    if fHintMap.ComponentIdentifier = AHintIdentifier then
    begin
      result := fHintMap;
      break;
    end;
  end;
end;

procedure TOPPHelpMapSet.MergeMaps(AList: TList<TOPPHelpMap>);
var
  fItem: TOPPHelpMap;
  fDictionary: TDictionary<String, TOPPHelpMap>;
begin
  if not assigned(AList) then
    exit;

  fDictionary := TDictionary<String, TOPPHelpMap>.Create;
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

  fList.Pack;
end;

end.
