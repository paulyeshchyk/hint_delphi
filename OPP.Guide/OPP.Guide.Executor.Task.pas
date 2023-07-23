unit OPP.Guide.Executor.Task;

interface

uses
  System.Classes, System.Threading,
  OPP_Guide_Executor,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable;

type
  TOPPGuideExecutorTask = class(TTask)
  private
  public
    class function RunOnly(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPExecutorStateCallback; ACompletion: TOPPGuideExecutorCompletion): Boolean; static;
  end;

implementation

uses

  OPP.Guide.Executor,

  OPP.Help.Log,
  OPP.Guide.Executor.Stream,
  OPP.Guide.Executor.RunState.Helper,
  OPP_Guide_Executor_State;

{ TOPPGuideExecutorTask }

class function TOPPGuideExecutorTask.RunOnly(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPExecutorStateCallback; ACompletion: TOPPGuideExecutorCompletion): Boolean;
begin

  ADataprovider.GetScriptedStream(AObject,
    procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
    begin
      if not Assigned(AStream) then
      begin
        if Assigned(AOnScriptConsoleLogOutput) then
          AOnScriptConsoleLogOutput(TOPPGuideExecutorRunState.error('', 'Stream is nil'));
        if Assigned(ACompletion) then
          ACompletion(AObject, TOPPGuideExecutorRunState.error('', 'stream is nil'));
        exit;
      end;

      TOPPStreamHelper.RunScript(AStream, AScripter, AIdentifiable,
        procedure(AState: TOPPGuideExecutorRunState)
        begin
          if Assigned(AOnScriptConsoleLogOutput) then
            AOnScriptConsoleLogOutput(AState);

          case AState.value of
            rsvFinished, rsvError:
              begin
                if Assigned(ACompletion) then
                  ACompletion(AObject, AState);
              end;
          end;
        end);
    end);

  result := true;
end;

end.
