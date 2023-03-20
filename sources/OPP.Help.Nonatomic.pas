unit OPP.Help.Nonatomic;

interface

type
  TOPPHelpKeyword = class
  private
    fBookmarkID: String;
    fSearchPattern: String;
    fPage: String;
  public
    function hashValue(): Integer;
    property bookmarkID: String read fBookmarkID write fbookmarkID;
    property searchPattern: String read fSearchPattern write fSearchPattern;
    property page: String read fPage write fPage;
  end;

implementation

function TOPPHelpKeyword.hashValue(): Integer;
begin
  result := 0;
end;

end.
