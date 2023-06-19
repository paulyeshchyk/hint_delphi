unit OPP.Guide.Scripter;

interface

uses
  System.Classes;

type

  IOPPGuideScripter = interface
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream): Variant; overload;
  end;

  TOPPGuideScripter = class(TInterfacedObject, IOPPGuideScripter)
  public
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream): Variant; overload;
  end;

implementation

uses
  //dcscript, dcPascal,
  System.SysUtils;

{ TOPPGuideScripter }

function TOPPGuideScripter.RunScript(AScriptText: String): Variant;
//var
//  fDCScripter: TDCScripter;
begin
//  fDCScripter := TDCScripter.Create(nil);
//  try
//    fDCScripter.Language := 'DelphiScript';
//    fDCScripter.UseModule := true;
//    fDCScripter.UseExceptions := true;
//    fDCScripter.Project := THandle(fDCScripter);
//    fDCScripter.Script.Text := AScriptText;
//    try
//      //result := fDCScripter.CallNoParamsMethod('Execute');
//      result := fDCScripter.DispatchMethod('Execute',[998]);
//    except
//      on E: Exception do
//      begin
//        //
//      end;
//    end;
//  finally
//    fDCScripter.Free;
//  end;

end;

function TOPPGuideScripter.RunScript(AStream: TMemoryStream): Variant;
//var
//  fDCScripter: TDCScripter;
begin
//  fDCScripter := TDCScripter.Create(nil);
//  try
//    fDCScripter.Language := 'DelphiScript';
//    fDCScripter.UseModule := true;
//    fDCScripter.UseExceptions := true;
//    fDCScripter.Project := THandle(fDCScripter);
//    fDCScripter.FilerID := 'DB';
//
//    AStream.Position := 0;
//    fDCScripter.Script.LoadFromStream(AStream);
//    try
//      result := fDCScripter.CallNoParamsMethod('Execute');
//    except
//      on E: Exception do
//      begin
//        //
//      end;
//    end;
//  finally
//    fDCScripter.Free;
//  end;
end;

end.
