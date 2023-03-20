unit OPP.Help.Nonatomic;

interface

type
  TOPPKeywordType = (ktAny, ktBookmark, ktSearch, ktPage);

  TOPPHelpIdentifier = class
  private
    fValue: String;
    fKeywordType: TOPPKeywordType;
  public
    function hashValue(): Integer;
    property value: String read fValue write fValue;
    property keywordType: TOPPKeywordType read fKeywordType write fKeywordType;
  end;

implementation

function TOPPHelpIdentifier.hashValue(): Integer;
begin
  result := 0;
end;

end.
