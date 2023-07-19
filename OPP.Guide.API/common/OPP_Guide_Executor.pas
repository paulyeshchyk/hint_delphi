unit OPP_Guide_Executor;

interface

uses
  DBClient,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_Executor_State;

type

  TOPPExecutorStateCallback = reference to procedure(AState: TOPPGuideExecutorRunState);

  TOPPIdentifiableClone = reference to function(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;

  IOPPGuideExecutor = interface(IUnknown)
    function compile(dataset: TClientDataset; Identifiable: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
    function run(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean; overload;
    function runSubs(dataset: TClientDataset; AFilter: String; Scripter: IOPPGuideScripter; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPExecutorStateCallback): Boolean;
  end;

  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  public
    class function IdleState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;static;
    class function StartedState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;static;
    class function ProgressState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;static;
    class function FinishState(AStepIdentifier, ADescription: String; AResult: String): TOPPGuideExecutorRunState;static;
    class function ErrorState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;static;
  end;

implementation

{ TOPPGuideExecutorRunStateHelper }

class function TOPPGuideExecutorRunStateHelper.ErrorState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvError;
  result.stepIdentifier := AStepIdentifier;
  result.userInfo := ADescription;
  result.executionResult := '';
end;

class function TOPPGuideExecutorRunStateHelper.FinishState(AStepIdentifier, ADescription, AResult: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvFinished;
  result.stepIdentifier := AStepIdentifier;
  result.userInfo := ADescription;
  result.executionResult := AResult;
end;

class function TOPPGuideExecutorRunStateHelper.IdleState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvIdle;
  result.stepIdentifier := AStepIdentifier;
  result.userInfo := ADescription;
  result.executionResult := '';
end;

class function TOPPGuideExecutorRunStateHelper.ProgressState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvProgress;
  result.stepIdentifier := AStepIdentifier;
  result.userInfo := ADescription;
  result.executionResult := '';
end;

class function TOPPGuideExecutorRunStateHelper.StartedState(AStepIdentifier, ADescription: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvStarted;
  result.stepIdentifier := AStepIdentifier;
  result.userInfo := ADescription;
  result.executionResult := '';
end;

end.
