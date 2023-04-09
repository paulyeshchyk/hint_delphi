unit OPP.Help.Log;

interface

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning, lmDebug, lmFlow);

  TOPPHelpLog = class
  private
    procedure Log(AString: String; postfix: String = ''; messageType: TOPPLogMessageType = lmInfo);
  public
    procedure Error(AString: String; AFlowName: String = '');
    procedure Warning(AString: String; AFlowName: String = '');
    procedure Debug(AString: String; AFlowName: String = '');
    procedure Flow(AString: String; AFlowName: String = '');
  end;

function eventLogger: TOPPHelpLog;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  WinAPI.Windows,
  OPP.Help.System.Str;

resourcestring
  SFlow = 'Flow';
  SFlowTemplate = '[%s]: %s';
  SWarningNoFlowTemplate = '[Warning]: %s';
  SInfoNoFlowTemplate = '[Info]: %s';
  SDebugNoFlowTemplate = '[Debug]: %s';
  SErrorNoFlowTemplate = '[Error]: %s';
  SErrorNoFlowPrefix = '%s';
  SErrorAndFlowPrefix = '[%s] - %s';

procedure TOPPHelpLog.Debug(AString: string; AFlowName: String = '');
begin
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmDebug)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmDebug);
end;

procedure TOPPHelpLog.Flow(AString: string; AFlowName: String);
begin
  self.Log(AString, AFlowName, lmFlow);
end;

procedure TOPPHelpLog.Error(AString: string; AFlowName: String = '');
begin
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmError)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmError);
end;

procedure TOPPHelpLog.Warning(AString: string; AFlowName: String = '');
begin
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmWarning)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmWarning);
end;

procedure TOPPHelpLog.Log(AString: string; postfix: String; messageType: TOPPLogMessageType);
var
  outresult: String;
  completeString: String;
begin

  case messageType of
    lmDebug:
      outresult := Format(SDebugNoFlowTemplate, [AString]);
    lmInfo:
      outresult := Format(SInfoNoFlowTemplate, [AString]);
    lmError:
      outresult := Format(SErrorNoFlowTemplate, [AString]);
    lmWarning:
      outresult := Format(SWarningNoFlowTemplate, [AString]);
    lmFlow:
      begin
        if length(postfix) = 0 then
          completeString := SFlow
        else
          completeString := postfix;
        outresult := Format(SFlowTemplate, [completeString, AString]);
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
