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
    class function CompileScript(AStream: TStream; AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPGuideAPIExecutionStateCallback): Boolean; static;
    class function RunScript(AStream: TStream; AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPGuideAPIExecutionStateCallback): Boolean; static;
  end;

implementation

uses
  System.SysUtils,
  Variants,
  OPP.Guide.API.Executor.RunStateHelper;

{ TOPPStreamHelper }

class function TOPPStreamHelper.CompileScript(AStream: TStream; AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPGuideAPIExecutionStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
  fState: TOPPGuideAPIExecutionState;
begin

  System.Assert(Assigned(AStream), 'Stream is nil');

  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  fState := TOPPGuideAPIExecutionState.started(VarToStr(userInfo));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        fState := TOPPGuideAPIExecutionState.finished('', VarToStr(fScriptExecutionResult));
      except
        on E: Exception do
        begin
          fState := TOPPGuideAPIExecutionState.error('', E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do
        fState := TOPPGuideAPIExecutionState.error('', E.Message);
    end;
  finally
    ss.Free;

    if Assigned(ALogOutputCompletion) then
      ALogOutputCompletion(fState);
  end;
  result := true;
end;

class function TOPPStreamHelper.RunScript(AStream: TStream; AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPGuideAPIExecutionStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
  fState: TOPPGuideAPIExecutionState;
begin
  AStream.Position := 0;
  AStream.Read(fScriptSize, SizeOf(fScriptSize));

  fState := TOPPGuideAPIExecutionState.started(stepInfo.IdentifierFieldValue);

  ss := TStringStream.Create;
  try
    ss.CopyFrom(AStream, fScriptSize);
    try

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, stepInfo);
        fState := TOPPGuideAPIExecutionState.finished(stepInfo.IdentifierFieldValue, VarToStr(fScriptExecutionResult));
      except
        on E: Exception do
        begin
          fState := TOPPGuideAPIExecutionState.error(stepInfo.IdentifierFieldValue, E.Message);
        end;
      end;
      { --- }
    except
      on E: Exception do
        fState := TOPPGuideAPIExecutionState.error(stepInfo.IdentifierFieldValue, E.Message);
    end;
  finally
    if Assigned(ALogOutputCompletion) then
      ALogOutputCompletion(fState);

    ss.Free;
  end;

  result := true;
end;

end.
