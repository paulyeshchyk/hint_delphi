unit OPP.Guide.Scripter;

interface

uses System.Classes, OPP_Guide_API;


type
  IOPPGuideScripter = interface
    function RunScript(AStrings: TStrings): Variant; overload;
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream; AIdentifiable: IOPPGuideAPIIdentifiable): Variant; overload;
    function CompileScript(AStream: TMemoryStream): Variant;
  end;

implementation

end.
