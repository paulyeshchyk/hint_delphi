unit OPP.Help.Log;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OPP.Stream.Observer;

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning, lmDebug, lmFlow);

  IOPPHelpLog = interface
    procedure Error(AString: String; AFlowName: String = ''); overload;
    procedure Error(AError: Exception; AFlowName: String = ''); overload;
    procedure Warning(AString: String; AFlowName: String = '');
    procedure Debug(AString: String; AFlowName: String = '');
    procedure Flow(AString: String; AFlowName: String = '');
  end;

  TOPPHelpLog = class(TInterfacedObject, IOPPHelpLog, IOPPStreamable)
  private
    fStreamObservers: TList<IOPPStreamObserver>;
    fStream: TStream;
    procedure Log(AString: String; postfix: String = ''; messageType: TOPPLogMessageType = lmInfo);
  public

    constructor Create;
    destructor Destroy; override;
    procedure Error(AString: String; AFlowName: String = ''); overload;
    procedure Error(AError: Exception; AFlowName: String = ''); overload;
    procedure Warning(AString: String; AFlowName: String = '');
    procedure Debug(AString: String; AFlowName: String = '');
    procedure Flow(AString: String; AFlowName: String = '');
    // IOPPStreamable
    procedure RegisterObserver(AObserver: IOPPStreamObserver);
    procedure UnregisterObserver(AObserver: IOPPStreamObserver);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
    function GetStream: TStream;
  end;

function eventLogger: TOPPHelpLog;

implementation

uses
  System.SyncObjs,
  WinAPI.Windows,
  OPP.Help.System.Str,
  OPP.Output.Console;

resourcestring
  SFlow = 'Flow';
  SFlowTemplate = '[%s]: %s';
  SWarningNoFlowTemplate = '[Warning]: %s';
  SInfoNoFlowTemplate = '[Info]: %s';
  SDebugNoFlowTemplate = '[Debug]: %s';
  SErrorNoFlowTemplate = '[Error]: %s';
  SErrorNoFlowPrefix = '%s';
  SErrorAndFlowPrefix = '[%s] - %s';

constructor TOPPHelpLog.Create;
begin
  inherited;
  fStream := TMemoryStream.Create;
  fStreamObservers := TList<IOPPStreamObserver>.Create;
end;

procedure TOPPHelpLog.Debug(AString: string; AFlowName: String = '');
begin
  exit;
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmDebug)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmDebug);
end;

procedure TOPPHelpLog.Flow(AString: string; AFlowName: String);
begin
  self.Log(AString, AFlowName, lmFlow);
end;

function TOPPHelpLog.GetStream: TStream;
begin
  result := fStream;
end;

destructor TOPPHelpLog.Destroy;
begin
  fStream.Free;
  fStreamObservers.Clear;
  fStreamObservers.Free;
  inherited;
end;

procedure TOPPHelpLog.DidChangeStream(AStream: TStream);
var
  fObserver: IOPPStreamObserver;
begin
  for fObserver in fStreamObservers do
  begin
    fObserver.DidChangeStream(fStream);
  end;
end;

procedure TOPPHelpLog.Error(AString: string; AFlowName: String = '');
begin
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmError)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmError);
end;

procedure TOPPHelpLog.Error(AError: Exception; AFlowName: String = '');
begin
  if not assigned(AError) then
    exit;
  Error(AError.message, AFlowName);
end;

procedure TOPPHelpLog.Warning(AString: string; AFlowName: String = '');
begin
  if Length(AFlowName) > 0 then
    self.Log(Format(SErrorAndFlowPrefix, [AFlowName, AString]), '', lmWarning)
  else
    self.Log(Format(SErrorNoFlowPrefix, [AString]), '', lmWarning);
end;

procedure TOPPHelpLog.WillChangeStream(AStream: TStream);
var
  fObserver: IOPPStreamObserver;
begin
  for fObserver in fStreamObservers do
  begin
    fObserver.WillChangeStream(fStream);
  end;
end;

procedure TOPPHelpLog.Log(AString: string; postfix: String; messageType: TOPPLogMessageType);
var
  outresult: String;
  completeString: String;
  fUTF8String: UTF8String;
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
        if Length(postfix) = 0 then
          completeString := SFlow
        else
          completeString := postfix;
        outresult := Format(SFlowTemplate, [completeString, AString]);
      end;
  end;

  fUTF8String := UTF8Encode(outResult);

  WillChangeStream(fStream);
  fStream.WriteString(fUTF8String);
  DidChangeStream(fStream);

end;

procedure TOPPHelpLog.RegisterObserver(AObserver: IOPPStreamObserver);
begin
  if not assigned(AObserver) then
    exit;
  if fStreamObservers.IndexOf(AObserver) = -1 then
    fStreamObservers.Add(AObserver);
  AObserver.StartListenStream(fStream);
end;

procedure TOPPHelpLog.UnregisterObserver(AObserver: IOPPStreamObserver);
begin
  if not assigned(AObserver) then
    exit;
  AObserver.StopListenStream(fStream);
  if fStreamObservers.IndexOf(AObserver) <> -1 then
    fStreamObservers.Remove(AObserver);
end;

{ ------------ }
var
  fInfoLogLock: TCriticalSection;
  fInfoLog: TOPPHelpLog;

function eventLogger: TOPPHelpLog;
begin
  fInfoLogLock.Acquire;
  try
    if not assigned(fInfoLog) then
    begin
      fInfoLog := TOPPHelpLog.Create();
    end;
    result := fInfoLog;
  finally
    fInfoLogLock.Release;
  end;
end;

var
  fConsoleOutput: IOPPStreamObserver;
  fFileOutput: IOPPStreamObserver;


initialization

fInfoLogLock := TCriticalSection.Create;

fConsoleOutput := TOPPConsoleOutput.Create;
eventLogger.RegisterObserver(fConsoleOutput);

fFileOutput := TOPPFileOutput.Create;
eventLogger.RegisterObserver(fFileOutput);

finalization

eventLogger.UnregisterObserver(fFileOutput);
fFileOutput := nil;

eventLogger.UnregisterObserver(fConsoleOutput);
fConsoleOutput := nil;

fInfoLogLock.Free;

end.
