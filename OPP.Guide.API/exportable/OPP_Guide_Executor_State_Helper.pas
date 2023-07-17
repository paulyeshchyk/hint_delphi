unit OPP_Guide_Executor_State_Helper;

interface

uses
  OPP_Guide_Executor_State;

type
  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  private
    function GetDescription: String;
  public
    property Description: String read GetDescription;
  end;

  TOPPGuideExecutorRunStateValueHelper = record helper for TOPPGuideExecutorRunStateValue
  private
    function GetDescription: String;
  public
    property Description: String read GetDescription;
  end;

implementation

uses
  SysUtils;

{ TOPPGuideExecutorRunStateHelper }

function TOPPGuideExecutorRunStateHelper.GetDescription: String;
begin
  result := Format('Step [%s] has state [%s] execution result [%s]', [stepIdentifier, value.Description, executionResult]);
end;

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

end.
