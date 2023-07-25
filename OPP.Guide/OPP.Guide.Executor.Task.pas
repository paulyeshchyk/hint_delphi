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
  TOPPGuideExecutorCompletion = TProc<IOPPGuideAPIIdentifiable, TOPPGuideAPIExecutionState>;

  TOPPGuideExecutorTask = class(TTask)
  private
  public
    class function CreateTask(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; ACompletion: TOPPGuideExecutorCompletion): ITask; static;
  end;

implementation

uses
  OPP.Guide.Executor,

  OPP.Help.Log,
  OPP.Guide.Executor.Stream,
  OPP.Guide.API.Executor.RunStateHelper;

{ TOPPGuideExecutorTask }

class function TOPPGuideExecutorTask.CreateTask(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; ACompletion: TOPPGuideExecutorCompletion): ITask;
begin

  result := TTask.Create(
    procedure()
    begin
      ADataprovider.LoadScriptContent(AObject,
        procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
        var
          fScriptRunResult : TOPPGuideAPIExecutionState;
        begin
          System.Assert(Assigned(AStream),'stream is nil');

          if Assigned(ACompletion) then
            ACompletion(AObject, TOPPGuideAPIExecutionState.started(AObject.IdentifierFieldValue));

          { --- }
          fScriptRunResult := TOPPStreamHelper.RunScript(AStream, AScripter, AIdentifiable);
          { --- }

          if Assigned(ACompletion) then
            ACompletion(AObject, fScriptRunResult);

        end);
    end);
end;

end.
