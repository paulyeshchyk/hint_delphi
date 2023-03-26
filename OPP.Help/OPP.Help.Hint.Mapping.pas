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
  public
    constructor Create(const AIdentifier: TOPPHelpHintMapIdentifier; const APredicate: TOPPHelpPredicate);

    property Predicate: TOPPHelpPredicate read fPredicate write fPredicate;
    property identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
  end;

  TOPPHelpHintMapSet = class(TObject)
  private
    fList: TList<TOPPHelpHintMap>;
  public
    constructor Create(AList: TList<TOPPHelpHintMap> = nil);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintMap;

    procedure AddMaps(AList: TList<TOPPHelpHintMap>);
    procedure MergeMaps(AList: TList<TOPPHelpHintMap>);
    property list: TList<TOPPHelpHintMap> read fList;
  end;

implementation

constructor TOPPHelpHintMap.Create(const AIdentifier: TOPPHelpHintMapIdentifier; const APredicate: TOPPHelpPredicate);
begin
  fIdentifier := AIdentifier;
  fPredicate := APredicate;
end;

constructor TOPPHelpHintMapSet.Create(AList: TList<OPP.Help.Hint.Mapping.TOPPHelpHintMap> = nil);
begin
  fList := TList<TOPPHelpHintMap>.Create;
  if assigned(AList) then
    fList.AddRange(AList);
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
begin
  if not assigned(AList) then
    exit;

  for fItem in AList do
  begin
    fList.Add(fItem);
  end;

end;

end.
