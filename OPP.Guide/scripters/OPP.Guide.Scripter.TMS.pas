unit OPP.Guide.Scripter.TMS;

interface

uses
  OPP.Guide.Scripter,

  System.Classes,
  System.Variants,
  System.SysUtils,

  Vcl.ScripterInit,

  WinAPI.ActiveX,
  WinAPI.Windows,
  atScript;

type

  TOPPGuideScripterTMS = class(TInterfacedObject, IOPPGuideScripter)
  private
    procedure scripterror(Sender: TObject; const ExcepInfo: TExcepInfo; CharNo, LineNo: integer; var Handled: boolean);
  public
    function RunScript(AStrings: TStrings): Variant; overload;
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream): Variant; overload;
  end;

  TRunProcessCompletion = reference to procedure(ARunResultType: Exception);


implementation

uses
//  ap_Classes,

  atScripter,
  atPascal,
  Vcl.Dialogs,

  OPP.Guide.Context.TaTWrapper,
  OPP.Guide.API.ContextLibrary,
  OPP.Guide.API.SampleLibrary;

{ TOPPGuideScripterTMS }

function TOPPGuideScripterTMS.RunScript(AScriptText: String): Variant;
var
  fStrings: TStringList;
begin

  fStrings := TStringList.Create;
  try
    fStrings.Add(AScriptText);
    self.RunScript(fStrings);
  finally
    fStrings.free;
  end;
end;

function TOPPGuideScripterTMS.RunScript(AStream: TMemoryStream): Variant;
begin

end;

function TOPPGuideScripterTMS.RunScript(AStrings: TStrings): Variant;
var
  fScripter: TatPascalScripter;
begin
  fScripter := TatPascalScripter.Create(nil);
  try
    fScripter.AddLibrary(TExampleLibrary);
    fScripter.AddLibrary(TAPIContextLibrary);
    fScripter.AddLibrary(TOPPGuideCoreLibrary);

//    with fScripter.AddDelphiClass(TOPPGuideContext) do begin
//      DefineMethod('test',0,tkNone,nil,tcontexttest);
//    end;
//
    fScripter.SourceCode := AStrings;
    fScripter.Compile;
    fScripter.Execute(0);
  finally
    fScripter.free;
  end;
end;

procedure TOPPGuideScripterTMS.scripterror(Sender: TObject; const ExcepInfo: TExcepInfo; CharNo, LineNo: integer; var Handled: boolean);
begin

end;

end.
