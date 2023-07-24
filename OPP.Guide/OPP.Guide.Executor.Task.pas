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
    class function RunOnly(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; ACompletion: TOPPGuideExecutorCompletion): Boolean; static;
  end;

implementation

uses

  OPP.Guide.Executor,

  OPP.Help.Log,
  OPP.Guide.Executor.Stream,
  OPP.Guide.API.Executor.RunStateHelper;

{ TOPPGuideExecutorTask }

class function TOPPGuideExecutorTask.RunOnly(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; AScripter: IOPPGuideScripter; ACompletion: TOPPGuideExecutorCompletion): Boolean;
begin

  ADataprovider.GetScriptedStream(AObject,
    procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
    begin
      if not Assigned(AStream) then
      begin
        if Assigned(ACompletion) then
          ACompletion(AObject, TOPPGuideAPIExecutionState.error(AObject.IdentifierFieldValue, 'stream is nil'));
        exit;
      end;

      if Assigned(ACompletion) then
        ACompletion(AObject, TOPPGuideAPIExecutionState.started(AObject.IdentifierFieldValue));

      TOPPStreamHelper.RunScript(AStream, AScripter, AIdentifiable,
        procedure(AState: TOPPGuideAPIExecutionState)
        begin
          if Assigned(ACompletion) then
            ACompletion(AObject, AState);
        end);
    end);

  result := true;
end;

end.
