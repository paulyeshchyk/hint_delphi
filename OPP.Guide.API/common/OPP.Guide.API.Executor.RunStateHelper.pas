unit OPP.Guide.API.Executor.RunStateHelper;

interface

uses OPP_Guide_API;

type
  TOPPGuideExecutorRunStateValueHelper = record helper for TOPPGuideExecutorRunStateValue
  private
    function GetDescription: String;
  public
    property Description: String read GetDescription;
  end;

  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  private
    function GetDescription: String;
  public
    class function error(const identifier: String; const text: String): TOPPGuideExecutorRunState; static;
    class function finished(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;
    class function idle(const identifier: String): TOPPGuideExecutorRunState; static;
    class function progress(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;
    class function started(const identifier: String; const text: String = ''): TOPPGuideExecutorRunState; static;

    property Description: String read GetDescription;
    function StateName: String;
  end;

implementation

uses
  System.SysUtils;

{ TOPPGuideExecutorRunStateValueHelper }

function TOPPGuideExecutorRunStateValueHelper.GetDescription: String;
begin
  case self of
    rsvIdle:
      result := 'Idle';
    rsvStarted:
      result := 'Started';
    rsvProgress:
      result := 'Progress';
    rsvFinished:
      result := 'Finished';
    rsvError:
      result := 'Error';
  end;
end;

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

function TOPPGuideExecutorRunStateHelper.GetDescription: String;
begin
  result := Format('Step [%s] has state [%s] and result [%s]',[self.stepIdentifier, self.value.Description, self.executionResult]);
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
