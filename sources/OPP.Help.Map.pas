unit OPP.Help.Map;

interface

uses
  SysUtils, Classes, JSON, DBXJSON, DBXJSONReflect;

type
  TOPPHelpMap = class
  private
    fHelpKeyword: String;
    fSearchPattern: String;
  public
    class function add(HelpKeyword: String;SearchPattern: String): TOPPHelpMap;

    property HelpKeyword: String read fHelpKeyword write fHelpKeyword;
    property SearchPattern: String read fSearchPattern write fSearchPattern;
  end;

implementation

class function TOPPHelpMap.add(HelpKeyword: string; SearchPattern: string): TOPPHelpMap;
begin
  result := TOPPHelpMap.Create;
  result.HelpKeyword := HelpKeyword;
  result.SearchPattern := SearchPattern;
end;

end.
