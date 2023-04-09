unit OPP.Help.System.Types;

interface

type
  TOPPKeywordType = (ktSearch = 0, ktPage = 1, ktBookmark = 2, ktAny = 3);

  TOPPKeywordTypeHelper = record helper for TOPPKeywordType
  public
    function asString(): String;
  end;

implementation

{ TOPPHelpPreviewFormStateHelper }

function TOPPKeywordTypeHelper.asString: String;
begin
  case self of
    ktSearch:
      result := 'Search';
    ktPage:
      result := 'Page';
    ktBookmark:
      result := 'Bookmark';
    ktAny:
      result := 'Any'
  end;
end;

end.
