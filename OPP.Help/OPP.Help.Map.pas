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
  public
    constructor Create; overload;
    constructor Create(AIdentifier: String); overload;
    destructor Destroy; override;
    property ComponentIdentifier: TOPPHelpHintMapIdentifier read fComponentIdentifier write fComponentIdentifier;
    property Identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
    property IsValid: Boolean read GetIsValid;
    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
  end;

  TOPPHelpMapSetList = TList<TOPPHelpMap>;

  TOPPHelpMapApplyDefaultsCompletion = reference to procedure(const AMap: POPPHelpMap);
  TOPPHelpMapCompletion = reference to procedure(const AMap: TOPPHelpMap);
  TOPPHelpMapParserJSONCallback = reference to procedure(Mapset: TOPPHelpMapSet; Error: Exception);

  TOPPHelpMapSet = class(TObject)
  private
    fList: TOPPHelpMapSetList;
    function GetList(): TOPPHelpMapSetList;
  public
    constructor Create(AList: TOPPHelpMapSetList = nil);
    destructor Destroy; override;
    procedure AddMap(AMap: TOPPHelpMap);
    procedure AddMaps(AList: TOPPHelpMapSetList);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
    procedure MergeMaps(AList: TOPPHelpMapSetList);
    property list: TOPPHelpMapSetList read GetList;
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
  FreeAndNil(fPredicate);
  inherited;
end;

function TOPPHelpMap.GetIsValid: Boolean;
begin
  result := Length(fComponentIdentifier) <> 0;
end;

constructor TOPPHelpMapSet.Create(AList: TOPPHelpMapSetList = nil);
begin
  inherited Create;
  fList := TOPPHelpMapSetList.Create;
  if assigned(AList) then
  begin
    fList.AddRange(AList);
  end;
end;

destructor TOPPHelpMapSet.Destroy;
begin

  fList.Clear;
  fList.Pack;
  fList.Free;

  inherited;
end;

procedure TOPPHelpMapSet.AddMap(AMap: TOPPHelpMap);
begin
  if not assigned(AMap) then
  begin
    exit;
  end;

  self.List.Add(AMap);
end;

procedure TOPPHelpMapSet.AddMaps(AList: TOPPHelpMapSetList);
var
  fItem: TOPPHelpMap;
begin
  if not assigned(AList) then
    exit;

  for fItem in AList do
  begin
    self.List.Add(fItem);
  end;

end;

function TOPPHelpMapSet.GetList: TOPPHelpMapSetList;
begin
  result := fList;
end;

function TOPPHelpMapSet.GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
var
  fHintMap: TOPPHelpMap;
begin
  result := nil;
  for fHintMap in self.list do
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

procedure TOPPHelpMapSet.MergeMaps(AList: TOPPHelpMapSetList);
var
  fItem: TOPPHelpMap;
  fDictionary: TDictionary<String, TOPPHelpMap>;
begin
  if not assigned(AList) then
    exit;

  fDictionary := TDictionary<String, TOPPHelpMap>.Create;
  try
    for fItem in self.List do
    begin
      if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
        fDictionary.Add(fItem.ComponentIdentifier, fItem);
    end;

    for fItem in AList do
    begin
      if fItem.IsValid then
      begin
        if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
          self.List.Add(fItem);
      end;
    end;
  finally
    fDictionary.Clear;
    fDictionary.Free;
  end;

  self.List.Pack;
end;

end.
