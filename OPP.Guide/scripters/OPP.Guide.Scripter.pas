unit OPP.Guide.Scripter;

interface

uses System.Classes;

type
  IOPPGuideScripter = interface
    function RunScript(AStrings: TStrings): Variant; overload;
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream): Variant; overload;
  end;

implementation

end.
