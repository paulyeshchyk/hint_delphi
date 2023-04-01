unit OPP.Help.Log;

interface

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning, lmDebug, lmFlow);

  TOPPHelpLog = class
  private
    procedure Log(AString: String; postfix: String = ''; messageType: TOPPLogMessageType = lmInfo);
  public
    procedure Error(AString: String);
    procedure Warning(AString: String);
    procedure Debug(AString: String);
    procedure Flow(AString: String; AFlowName: String = '');
  end;

function eventLogger: TOPPHelpLog;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  WinAPI.Windows,
  OPP.Help.System.Str;

procedure TOPPHelpLog.Debug(AString: string);
begin
  // self.Log(AString, '', lmDebug);
end;

procedure TOPPHelpLog.Flow(AString: string; AFlowName: String);
begin
  self.Log(AString, AFlowName, lmFlow);
end;

procedure TOPPHelpLog.Error(AString: string);
begin
  self.Log(AString, '', lmError);
end;

procedure TOPPHelpLog.Warning(AString: string);
begin
  self.Log(AString, '', lmWarning);
end;

procedure TOPPHelpLog.Log(AString: string; postfix: String; messageType: TOPPLogMessageType);
var
  outresult: String;
  completeString: String;
begin

  case messageType of
    lmDebug:
      outresult := Format('[Debug]: %s', [AString]);
    lmInfo:
      outresult := Format('[Info]: %s', [AString]);
    lmError:
      outresult := Format('[Error]: %s', [AString]);
    lmWarning:
      outresult := Format('[Warning]: %s', [AString]);
    lmFlow:
      begin
        if length(postfix) = 0 then
          completeString := 'Flow'
        else
          completeString := postfix;
        outresult := Format('[%s]: %s', [completeString, AString]);
      end;
  end;

  OutputDebugString(outresult.toWideChar);
end;

{ ------------ }
var
  fInfoLogLock: TCriticalSection;
  fInfoLog: TOPPHelpLog;

function eventLogger: TOPPHelpLog;
begin
  fInfoLogLock.Acquire;
  try
    if not Assigned(fInfoLog) then
    begin
      fInfoLog := TOPPHelpLog.Create;
    end;
    result := fInfoLog;
  finally
    fInfoLogLock.Release;
  end;
end;

initialization

fInfoLogLock := TCriticalSection.Create;

finalization

fInfoLogLock.Free;

end.
