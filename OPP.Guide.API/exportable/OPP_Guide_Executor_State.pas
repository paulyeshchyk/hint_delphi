unit OPP_Guide_Executor_State;

interface

type
//  TOPPGuideAPIContextStepState = (osIdle = 0, osRunning = 1, osError = 2, osUnknown = 3);

  TOPPGuideExecutorRunStateValue = (rsvIdle = 0, rsvStarted = 1, rsvProgress = 2, rsvFinished = 3, rsvError = -1);

  TOPPGuideExecutorRunState = record
    stepIdentifier: String;
    value: TOPPGuideExecutorRunStateValue;
    shortDescription: String;
    executionResult: String;
  end;

implementation

end.
