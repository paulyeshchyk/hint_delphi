unit OPP.Guide.Executor.Stream;

interface

uses
  System.Classes,

  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_Executor;

type
  TOPPStreamHelper = class
    class function CompileScript(AStream: TStream;AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;static;
    class function RunScript(AStream: TStream;AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;static;
  end;

implementation

uses
  System.SysUtils,
  Variants,
  OPP_Guide_Executor_State,
  OPP.Guide.Executor.RunState.Helper;

{ TOPPStreamHelper }

class function TOPPStreamHelper.CompileScript(AStream: TStream;AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
  fState: TOPPGuideExecutorRunState;
begin

  System.Assert(Assigned(AStream),'Stream is nil');

  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  fState := TOPPGuideExecutorRunState.started(VarToStr(userInfo));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        fState := TOPPGuideExecutorRunState.finished('', VarToStr(fScriptExecutionResult));
      except
        on E: Exception do
        begin
          fState := TOPPGuideExecutorRunState.error('', E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do
        fState := TOPPGuideExecutorRunState.error('', E.Message);
    end;
  finally
    ss.Free;

    if Assigned(ALogOutputCompletion) then
      ALogOutputCompletion(fState);
  end;
  result := true;
end;

class function TOPPStreamHelper.RunScript(AStream: TStream;AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
  fState: TOPPGuideExecutorRunState;
begin
  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  fState := TOPPGuideExecutorRunState.started(stepInfo.IdentifierValue);

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, stepInfo);
        fState := TOPPGuideExecutorRunState.finished(stepInfo.IdentifierValue, VarToStr(fScriptExecutionResult));
      except
        on E: Exception do
        begin
          fState := TOPPGuideExecutorRunState.error(stepInfo.IdentifierValue, E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do
        fState := TOPPGuideExecutorRunState.error(stepInfo.IdentifierValue, E.Message);
    end;
  finally
    if Assigned(ALogOutputCompletion) then
      ALogOutputCompletion(fState);

    ss.Free;
  end;

  result := true;
end;

end.
