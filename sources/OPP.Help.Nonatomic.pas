unit OPP.Help.Nonatomic;

interface

type
  TOPPHelpKeyword = record
    bookmarkID: String;
    searchPattern: String;
    page: String;
  public
    function hashValue(): Integer;
  end;

implementation

function TOPPHelpKeyword.hashValue(): Integer;
begin
  result := 0;
end;

end.
