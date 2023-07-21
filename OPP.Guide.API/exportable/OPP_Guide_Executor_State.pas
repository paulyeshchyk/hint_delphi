unit OPP_Guide_Executor_State;

interface

type
  TOPPGuideExecutorRunStateValue = (rsvIdle = 0, rsvStarted = 1, rsvProgress = 2, rsvFinished = 3, rsvError = -1);

  TOPPGuideExecutorRunState = record
    stepIdentifier: String;
    value: TOPPGuideExecutorRunStateValue;
    executionResult: String;
  end;

implementation


end.
