unit OPP.Help.Map;

interface

uses
  System.SysUtils,
  System.Generics.Collections, System.Generics.Defaults,

  OPP.Help.System.TList.Filter,

  OPP.Help.System.References,
  OPP.Help.Predicate;

type

  TOPPHelpMapList = class;

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

  TOPPHelpMapApplyDefaultsCompletion = reference to procedure(var AMap: TOPPHelpMap);
  TOPPHelpMapCompletion = reference to procedure(const AMap: TOPPHelpMap);
  TOPPHelpMapParserJSONCallback = reference to procedure(Mapset: TOPPHelpMapList; Error: Exception);

  TOPPHelpMapList = class(TFilteredList<TOPPHelpMap>)
  public
    constructor Create(AList: TOPPHelpMapList = nil);
    destructor Destroy; override;

    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
    procedure MergeMaps(AList: TOPPHelpMapList);
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

constructor TOPPHelpMapList.Create(AList: TOPPHelpMapList = nil);
begin
  inherited Create;
  if assigned(AList) then
  begin
    AddRange(AList);
  end;
end;

destructor TOPPHelpMapList.Destroy;
begin

  Clear;
  Pack;
  inherited;
end;

function TOPPHelpMapList.GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
var
  fHintMap: TOPPHelpMap;
begin
  result := nil;

  for fHintMap in self do
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

procedure TOPPHelpMapList.MergeMaps(AList: TOPPHelpMapList);
var
  fItem: TOPPHelpMap;
  fDictionary: TDictionary<String, TOPPHelpMap>;
begin
  if not assigned(AList) then
    exit;

  fDictionary := TDictionary<String, TOPPHelpMap>.Create;
  try
    for fItem in self do
    begin
      if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
        fDictionary.Add(fItem.ComponentIdentifier, fItem);
    end;

    for fItem in AList do
    begin
      if fItem.IsValid then
      begin
        if not fDictionary.ContainsKey(fItem.ComponentIdentifier) then
          self.Add(fItem);
      end;
    end;
  finally
    fDictionary.Clear;
    fDictionary.Free;
  end;

  self.Pack;
end;

end.
