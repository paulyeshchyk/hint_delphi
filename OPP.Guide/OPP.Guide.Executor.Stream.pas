unit OPP.Guide.Executor.Stream;

interface

uses
  System.Classes,

  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP.Guide.Executor.Task,
  OPP_Guide_API_Scripter;

type
  TOPPStreamHelper = class
    class function CompileScript(AStream: TStream; AScripter: IOPPGuideScripter; userInfo: OLEVariant): TOPPGuideAPIExecutionState; static;
    class function RunScript(AStream: TStream; AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable): TOPPGuideAPIExecutionState; static;
  end;

implementation

uses
  System.SysUtils,
  Variants,
  OPP.Guide.API.Executor.RunStateHelper;

{ TOPPStreamHelper }

class function TOPPStreamHelper.CompileScript(AStream: TStream; AScripter: IOPPGuideScripter; userInfo: OLEVariant): TOPPGuideAPIExecutionState;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
begin

  System.Assert(Assigned(AStream), 'Stream is nil');

  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        result := TOPPGuideAPIExecutionState.finished('', VarToStr(fScriptExecutionResult));
      except
        on E: Exception do
        begin
          result := TOPPGuideAPIExecutionState.error('', E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do begin
        result := TOPPGuideAPIExecutionState.error('', E.Message);
      end;
    end;
  finally
    ss.Free;
  end;
end;

class function TOPPStreamHelper.RunScript(AStream: TStream; AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable): TOPPGuideAPIExecutionState;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
begin
  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, stepInfo);

        result := TOPPGuideAPIExecutionState.started(stepInfo.IdentifierFieldValue);

      except
        on E: Exception do
        begin
          result := TOPPGuideAPIExecutionState.error(stepInfo.IdentifierFieldValue, E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do
      begin
        result := TOPPGuideAPIExecutionState.error(stepInfo.IdentifierFieldValue, E.Message);
      end;
    end;
  finally
    ss.Free;
  end;
end;

end.
