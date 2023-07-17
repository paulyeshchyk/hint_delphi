unit OPP.Help.System.Types;

interface

uses
  OPP_Help_API;

type

  TOPPKeywordTypeHelper = record helper for TOPPKeywordType
  public
    function asString(): String;
    class function FromString(AString: String): TOPPKeywordType;static;
  end;

implementation

uses
  System.SysUtils, AnsiStrings;

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

class function TOPPKeywordTypeHelper.FromString(AString: String): TOPPKeywordType;
begin

  case AnsiIndexStr(Uppercase(AString), ['SEARCH', 'PAGE', 'BOOKMARK']) of
    0: result := ktSearch;
    1: result := ktPage;
    2: result := ktBookmark;
    else result := ktAny
  end;
end;

end.
