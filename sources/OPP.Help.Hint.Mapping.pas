unit OPP.Help.Hint.Mapping;

interface

uses
  System.Generics.Collections,
  OPP.Help.Nonatomic;

type

  TOPPHelpHintMap = class(TObject)
  private
    fHelpKeyword: TOPPHelpKeyword;
    fFilename: string;
  public
    constructor Create(AHelpKeyword: TOPPHelpKeyword; AFileName: String);

    property helpKeyword: TOPPHelpKeyword read fHelpKeyword write fHelpKeyword;
    property filename: string read fFilename write fFilename;
  end;

  TOPPHelpHintMapSet = class(TObject)
  private
    fList: TList<TOPPHelpHintMap>;
  public
    property list: TList<TOPPHelpHintMap> read fList write fList;
    function GetMap(AHelpKeyword: TOPPHelpKeyword): TOPPHelpHintMap;
    constructor Create(AList: TList<TOPPHelpHintMap> = nil);
  end;

implementation

constructor TOPPHelpHintMap.Create(AHelpKeyword: TOPPHelpKeyword; AFileName: String);
begin
  fHelpKeyword := AHelpKeyword;
  fFilename := AFileName;
end;

constructor TOPPHelpHintMapSet.Create(AList: TList<OPP.Help.Hint.Mapping.TOPPHelpHintMap> = nil);
begin
  fList := TList<TOPPHelpHintMap>.Create;
  if assigned(AList) then
    fList.AddRange(AList);
end;

function TOPPHelpHintMapSet.GetMap(AHelpKeyword: TOPPHelpKeyword): TOPPHelpHintMap;
var
  item: TOPPHelpHintMap;
begin
  result := nil;
  for item in fList do
  begin
    if item.helpKeyword.hashValue = AHelpKeyword.hashValue then
    begin
      result := item;
      break;
    end;
  end;
end;

end.
