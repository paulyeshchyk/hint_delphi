unit OPP.Help.Hint.Mapping;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic;

type

  TOPPHelpHintMap = class(TObject)
  private
    fPredicate: TOPPHelpPredicate;
    fFilename: string;
  public
    constructor Create(AHelpKeyword: TOPPHelpPredicate; AFileName: String);

    property predicate: TOPPHelpPredicate read fPredicate write fPredicate;
    property filename: string read fFilename write fFilename;
  end;

  TOPPHelpHintMapSet = class(TObject)
  private
    fList: TList<TOPPHelpHintMap>;
  public
    constructor Create(AList: TList<TOPPHelpHintMap> = nil);
    function GetMap(AHelpKeyword: TOPPHelpPredicate): TOPPHelpHintMap;

    property list: TList<TOPPHelpHintMap> read fList write fList;
  end;

implementation

constructor TOPPHelpHintMap.Create(AHelpKeyword: TOPPHelpPredicate; AFileName: String);
begin
  fPredicate := AHelpKeyword;
  fFilename := AFileName;
end;

constructor TOPPHelpHintMapSet.Create(AList: TList<OPP.Help.Hint.Mapping.TOPPHelpHintMap> = nil);
begin
  fList := TList<TOPPHelpHintMap>.Create;
  if assigned(AList) then
    fList.AddRange(AList);
end;

function TOPPHelpHintMapSet.GetMap(AHelpKeyword: TOPPHelpPredicate): TOPPHelpHintMap;
var
  item: TOPPHelpHintMap;
begin
  result := nil;
  for item in fList do
  begin
    if item.predicate.hashValue = AHelpKeyword.hashValue then
    begin
      result := item;
      break;
    end;
  end;
end;

end.
