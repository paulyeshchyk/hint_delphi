unit OPP.Guide.Executor.Task;

interface

uses
  System.Classes,
  System.Threading,
  System.SysUtils,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Dataprovider,
  OPP_Guide_API_Scripter;

type
  TOPPExecutorStateCallback = TProc<TOPPGuideExecutorRunState>;
  TOPPGuideExecutorCompletion = TProc<IOPPGuideAPIIdentifiable, TOPPGuideExecutorRunState>;

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
  OPP.Guide.API.Executor.RunStateHelper;

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

      if Assigned(AOnScriptConsoleLogOutput) then
        AOnScriptConsoleLogOutput(TOPPGuideExecutorRunState.started(AObject.IdentifierValue));

      TOPPStreamHelper.RunScript(AStream, AScripter, AIdentifiable,
        procedure(AState: TOPPGuideExecutorRunState)
        begin
          if Assigned(AOnScriptConsoleLogOutput) then
            AOnScriptConsoleLogOutput(AState);

          if Assigned(ACompletion) then
            ACompletion(AObject, AState);
        end);
    end);

  result := true;
end;

end.
