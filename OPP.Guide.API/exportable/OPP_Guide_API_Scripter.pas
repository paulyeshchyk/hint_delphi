unit OPP_Guide_API_Scripter;

interface

uses
  System.Classes,
  OPP_Guide_API_Identifiable;

type
  IOPPGuideScripter = interface
    ['{795CF965-9560-43D0-B7A2-A5CCDAAABC24}']
    function RunScript(AStrings: TStrings): Variant; overload;
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream; AIdentifiable: IOPPGuideAPIIdentifiable): Variant; overload;
    function CompileScript(AStream: TMemoryStream): Variant;
  end;

implementation

end.
