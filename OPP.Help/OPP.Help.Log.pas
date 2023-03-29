unit OPP.Help.Log;

interface

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning);

  TOPPHelpLog = class
  public
    procedure Log(AString: String; messageType: TOPPLogMessageType = lmInfo);
  end;

function logger: TOPPHelpLog;

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
    lmInfo: outresult := Format('[Info]: %s',[AString]);
    lmError: outresult := Format('[Error]: %s',[AString]);
  end;
  OutputDebugString(outresult.toWideChar);
end;

{ ------------ }
var
  fLock: TCriticalSection;
  fLog: TOPPHelpLog;

function logger: TOPPHelpLog;
begin
  fLock.Acquire;
  try
    if not Assigned(fLog) then
    begin
      fLog := TOPPHelpLog.Create;
    end;
    result := fLog;
  finally
    fLock.Release;
  end;
end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
