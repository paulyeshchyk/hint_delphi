unit OPP.Help.Log;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  OPP.Help.Log.Output,
  OPP.Help.Log.Formatter;

type
  TOPPLogMessageType = (lmInfo, lmError, lmWarning, lmDebug, lmFlow);

  TOPPHelpLog = class(TInterfacedObject, IOPPHelpLog)
  private
    fSessionStart: TDateTime;
    fOutputs: TList<IOPPHelpLogOutput>;
  public
    constructor Create();
    destructor Destroy; override;
    procedure RegisterLogOutput(AOutput: IOPPHelpLogOutput);
    procedure Custom(const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
    procedure SessionStart(date: TDateTime);
    procedure SessionEnd(date: TDateTime);
  end;

  TOPPHelpLogHelper = class helper for TOPPHelpLog
    procedure Error(const AString: String; AFlowName: String = ''); overload;
    procedure Error(const AError: Exception; AFlowName: String = ''); overload;
    procedure Warning(const AString: String; AFlowName: String = '');
    procedure Debug(const AString: String; AFlowName: String = '');
    procedure Flow(const AString: String; AFlowName: String = '');
  end;

function eventLogger: TOPPHelpLog;

implementation

uses
  WinAPI.Windows,
  System.SyncObjs,
  OPP.Help.System.Str,
  OPP.Help.Log.OutputConsole,
  OPP.Help.Log.OutputFile;

constructor TOPPHelpLog.Create();
begin
  inherited Create;

  fOutputs := TList<IOPPHelpLogOutput>.Create;
  fSessionStart := Now();
end;

procedure TOPPHelpLog.Custom(const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
begin

  if not assigned(fOutputs) then
    exit;

  TThread.Queue(TThread.Current,
    procedure()
    var
      fDate: TOPPHelpLogDate;
      fOutput: IOPPHelpLogOutput;
    begin

      fDate.sessionDate := fSessionStart;
      fDate.messageDate := Now();
      try
        for fOutput in fOutputs do
        begin
          fOutput.WriteText(fDate, AText, AFlowName, AFormatterClass);
        end;
      except
        on E: Exception do
        begin
          OutputDebugString(E.message.toWideChar);
        end;
      end;
    end);
end;

destructor TOPPHelpLog.Destroy;
begin

  self.SessionEnd(fSessionStart);

  if assigned(fOutputs) then
    fOutputs.Clear;

  inherited;
end;

procedure TOPPHelpLog.RegisterLogOutput(AOutput: IOPPHelpLogOutput);
begin
  if assigned(AOutput) then
  begin
    fOutputs.Add(AOutput);
  end;
end;

procedure TOPPHelpLog.SessionStart(date: TDateTime);
begin
  self.fSessionStart := date;

  if not assigned(fOutputs) then
    exit;

  TThread.Queue(TThread.Current,
    procedure()
    var
      fOutput: IOPPHelpLogOutput;
    begin
      for fOutput in fOutputs do
        fOutput.StartSession(date);
    end)
end;

procedure TOPPHelpLog.SessionEnd(date: TDateTime);
begin
  if not assigned(fOutputs) then
    exit;

  TThread.Queue(TThread.Current,
    procedure()
    var
      fOutput: IOPPHelpLogOutput;
    begin
      for fOutput in fOutputs do
        fOutput.EndSession(date);
    end);
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
      fInfoLog.RegisterLogOutput(TOPPHelpLogOutputConsole.Create);
      fInfoLog.RegisterLogOutput(TOPPHelpLogOutputFile.Create);

      fInfoLog.SessionStart(Now());
    end;
    result := fInfoLog;
  finally
    fInfoLogLock.Release;
  end;
end;

{ TOPPHelpLogHelper }

procedure TOPPHelpLogHelper.Error(const AString: string; AFlowName: String = '');
begin
  self.Custom(AString, AFlowName, TOPPHelpErrorLogFormatter);
end;

procedure TOPPHelpLogHelper.Error(const AError: Exception; AFlowName: String = '');
begin
  if not assigned(AError) then
    exit;
  Error(AError.message, AFlowName);
end;

procedure TOPPHelpLogHelper.Warning(const AString: string; AFlowName: String = '');
begin
  self.Custom(AString, AFlowName, TOPPHelpWarningLogFormatter);
end;

procedure TOPPHelpLogHelper.Debug(const AString: string; AFlowName: String = '');
begin
  self.Custom(AString, AFlowName, TOPPHelpDebugLogFormatter);
end;

procedure TOPPHelpLogHelper.Flow(const AString: string; AFlowName: String);
begin
  self.Custom(AString, AFlowName, TOPPHelpFlowLogFormatter);
end;

initialization

fInfoLogLock := TCriticalSection.Create;

finalization

fInfoLogLock.Free;

end.
