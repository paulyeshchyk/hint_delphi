unit OPP.Help.Hint.Mapping;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic;

type

  TOPPHelpHintMap = class(TObject)
  private
    fPredicate: TOPPHelpPredicate;
    fIdentifier: TOPPHelpHintMapIdentifier;
  public
    constructor Create(AIdentifier: TOPPHelpHintMapIdentifier; APredicate: TOPPHelpPredicate);

    property predicate: TOPPHelpPredicate read fPredicate write fPredicate;
    property identifier: TOPPHelpHintMapIdentifier read fIdentifier write fIdentifier;
  end;

  TOPPHelpHintMapSet = class(TObject)
  private
    fList: TList<TOPPHelpHintMap>;
  public
    constructor Create(AList: TList<TOPPHelpHintMap> = nil);
    function GetMap(AHintIdentifier: TOPPHelpHintMapIdentifier): TOPPHelpHintMap;

    property list: TList<TOPPHelpHintMap> read fList write fList;
  end;

implementation

constructor TOPPHelpHintMap.Create(AIdentifier: TOPPHelpHintMapIdentifier; APredicate: TOPPHelpPredicate);
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

end.
