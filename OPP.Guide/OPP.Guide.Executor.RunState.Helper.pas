unit OPP.Guide.Executor.RunState.Helper;

interface

uses
  OPP_Guide_Executor_State;

type
  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  public
    class function error(const identifier: String; const text: String): TOPPGuideExecutorRunState; static;
    class function finished(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;
    class function idle(const identifier: String): TOPPGuideExecutorRunState; static;
    class function progress(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;
    class function started(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;
    function StateName: String;
  end;

implementation

uses
  System.SysUtils;

{ TOPPGuideExecutorRunStateHelper }

class function TOPPGuideExecutorRunStateHelper.error(const identifier: String; const text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvError;
  result.executionResult := StringReplace(text, 'Exception' + Chr(13) + Chr(10), '', [rfReplaceAll, rfIgnoreCase]);
end;

class function TOPPGuideExecutorRunStateHelper.finished(const identifier: String; const text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvFinished;
  result.executionResult := text;
end;

class function TOPPGuideExecutorRunStateHelper.idle(const identifier: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvIdle;
end;

class function TOPPGuideExecutorRunStateHelper.progress(const identifier: String; const text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvProgress;
  result.executionResult := text;
end;

class function TOPPGuideExecutorRunStateHelper.started(const identifier: String; const text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvStarted;
  result.executionResult := text;
end;

function TOPPGuideExecutorRunStateHelper.StateName: String;
begin
  case self.value of
    rsvIdle:
      result := 'rsvIdle';
    rsvStarted:
      result := 'rsvStarted';
    rsvProgress:
      result := 'rsvProgress';
    rsvFinished:
      result := 'rsvFinished';
    rsvError:
      result := 'rsvError';
  end;
end;

end.
