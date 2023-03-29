unit OPP.Help.Hint.Mapping;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic,
  OPP.Help.Predicate;

type

  TOPPHelpHintMap = class(TObject)
  private
    fPredicate: TOPPHelpPredicate;
    fIdentifier: TOPPHelpHintMapIdentifier;
    function GetIsValid: Boolean;
  public
    constructor Create(const AIdentifier: TOPPHelpHintMapIdentifier; const APredicate: TOPPHelpPredicate);

    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
    property identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
    property isValid: Boolean read GetIsValid;
  end;

  TOPPHelpHintMapSet = class(TObject)
  private
    fList: TList<TOPPHelpHintMap>;
    function GetList(): TList<TOPPHelpHintMap>;
  public
    constructor Create(AList: TList<TOPPHelpHintMap> = nil);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintMap;

    procedure AddMaps(AList: TList<TOPPHelpHintMap>);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    property list: TList<TOPPHelpHintMap> read GetList;
  end;

implementation

constructor TOPPHelpHintMap.Create(const AIdentifier: TOPPHelpHintMapIdentifier; const APredicate: TOPPHelpPredicate);
begin
  fIdentifier := AIdentifier;
  fPredicate := APredicate;
end;

function TOPPHelpHintMap.GetIsValid: Boolean;
begin
  result := Length(fIdentifier) <> 0;
end;

constructor TOPPHelpHintMapSet.Create(AList: TList<OPP.Help.Hint.Mapping.TOPPHelpHintMap> = nil);
begin
  fList := TList<TOPPHelpHintMap>.Create;
  if assigned(AList) then
  begin
    fList.AddRange(AList);
  end;
end;

function TOPPHelpHintMapSet.GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintMap;
var
  fHintMap: TOPPHelpHintMap;
begin
  result := nil;
  for fHintMap in fList do
  begin
    if fHintMap.identifier = AHintIdentifier then
    begin
      result := fHintMap;
      break;
    end;
  end;
end;

function TOPPHelpHintMapSet.GetList: TList<TOPPHelpHintMap>;
begin
  result := fList;
end;

procedure TOPPHelpHintMapSet.AddMaps(AList: TList<TOPPHelpHintMap>);
var
  fItem: TOPPHelpHintMap;
begin
  if not assigned(AList) then
    exit;

  for fItem in AList do
  begin
    fList.Add(fItem);
  end;

end;

procedure TOPPHelpHintMapSet.MergeMaps(AList: TList<TOPPHelpHintMap>);
var
  fItem: TOPPHelpHintMap;
  fDictionary: TDictionary<String, TOPPHelpHintMap>;
begin
  if not assigned(AList) then
    exit;

  fDictionary := TDictionary<String, TOPPHelpHintMap>.Create;
  for fItem in fList do
  begin
    if not fDictionary.ContainsKey(fItem.identifier) then
      fDictionary.Add(fItem.identifier, fItem);
  end;

  for fItem in AList do
  begin
    if fItem.isValid then
    begin
      if not fDictionary.ContainsKey(fItem.identifier) then
        fList.Add(fItem);
    end;
  end;

  fList.Pack;
end;

end.
