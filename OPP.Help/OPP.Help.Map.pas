unit OPP.Help.Map;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic,
  OPP.Help.Predicate;

type

  TOPPHelpMap = class(TObject)
  private
    fPredicate: TOPPHelpPredicate;
    fIdentifier: TOPPHelpHintMapIdentifier;
    function GetIsValid: Boolean;
  public
    property identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
    property isValid: Boolean read GetIsValid;
  end;

  TOPPHelpMapSet = class(TObject)
  private
    fList: TList<TOPPHelpMap>;
    function GetList(): TList<TOPPHelpMap>;
  public
    constructor Create(AList: TList<TOPPHelpMap> = nil);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;

    procedure AddMaps(AList: TList<TOPPHelpMap>);
    procedure MergeMaps(AList: TList<TOPPHelpMap>);
    property list: TList<TOPPHelpMap> read GetList;
  end;

implementation

function TOPPHelpMap.GetIsValid: Boolean;
begin
  result := Length(fIdentifier) <> 0;
end;

constructor TOPPHelpMapSet.Create(AList: TList<OPP.Help.Map.TOPPHelpMap> = nil);
begin
  fList := TList<TOPPHelpMap>.Create;
  if assigned(AList) then
  begin
    fList.AddRange(AList);
  end;
end;

function TOPPHelpMapSet.GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpMap;
var
  fHintMap: TOPPHelpMap;
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

function TOPPHelpMapSet.GetList: TList<TOPPHelpMap>;
begin
  result := fList;
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
