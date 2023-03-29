unit OPP.Help.Log;

interface

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning);

  TOPPHelpLog = class
  public
    procedure Log(AString: String; messageType: TOPPLogMessageType = lmInfo);
  end;

function eventLogger: TOPPHelpLog;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  WinAPI.Windows,
  OPP.Help.System.Str;

procedure TOPPHelpLog.Log(AString: string; messageType: TOPPLogMessageType);
var
  outresult: String;
begin

  case messageType of
    lmInfo:
      outresult := Format('[Info]: %s', [AString]);
    lmError:
      outresult := Format('[Error]: %s', [AString]);
    lmWarning:
      outresult := Format('[Warning]: %s', [AString]);
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
