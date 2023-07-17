unit OPP_Help_API;

interface

uses
  Classes;

type

  TOPPKeywordType = (ktSearch = 0, ktPage = 1, ktBookmark = 2, ktAny = 3);

  IOPPHelpPredicate = interface
    ['{BA2E6A8B-A7ED-4608-88D8-B661D32A0A26}']

    procedure SetValue(AValue: String);
    function GetValue: String;

    function GetKeywordType: Integer;
    procedure SetKeywordType(const Value: Integer);

    procedure SetFileName(const Value: String);
    function GetFileName: String;

    function GetIsRunnable: Boolean;

    function ReadFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
    function WriteToStream(AStream: TStream): Boolean;
  end;

implementation

end.
