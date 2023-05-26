unit OPP.Help.Log.OutputConsole;

interface

uses
  WinAPI.Windows,
  System.SysUtils,
  System.Classes,
  OPP.Help.System.Str,
  OPP.Help.Log.Formatter,
  OPP.Help.Log.Output;

type
  TOPPHelpLogOutputConsole = class(TInterfacedObject, IOPPHelpLogOutput)
  private
    function IsAcceptableFormatter(const AFormatterClass: TOPPHelpLogFormatterClass): Boolean;
  public
    procedure StartSession(date: TDateTime);
    procedure EndSession(date: TDateTime);
    procedure WriteText(const ADate: TOPPHelpLogDate; const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
  end;

implementation

{ TOPPConsoleLogOutput }

procedure TOPPHelpLogOutputConsole.EndSession(date: TDateTime);
var
  fDate: TOPPHelpLogDate;
begin
  fDate.sessionDate := date;
  fDate.messageDate := Now();
  WriteText(fDate, 'End Log session', '', nil);
end;

function TOPPHelpLogOutputConsole.IsAcceptableFormatter(const AFormatterClass: TOPPHelpLogFormatterClass): Boolean;
begin
  result := true;
  if Assigned(AFormatterClass) then
    result := not AFormatterClass.isDebug;
end;

procedure TOPPHelpLogOutputConsole.StartSession(date: TDateTime);
var
  fDate: TOPPHelpLogDate;
begin
  fDate.sessionDate := date;
  fDate.messageDate := Now();
  WriteText(fDate, 'Start Log session', '', nil);
end;

procedure TOPPHelpLogOutputConsole.WriteText(const ADate: TOPPHelpLogDate; const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
var
  fText: String;
  fWideChar: PWideChar;
  fFormatter: TOPPHelpDefaultLogFormatter;
begin
  if not IsAcceptableFormatter(AFormatterClass) then
    exit;

  if Assigned(AFormatterClass) then
  begin
    fFormatter := AFormatterClass.Create();
    try
      fFormatter.DateTimeFormatType := dtElapsed;
      fFormatter.FlowName := AFlowName;
      fFormatter.ForceLineBreak := false;
      fFormatter.date := ADate;
      fText := fFormatter.Format(AText);
    finally
      fFormatter.Free;
    end;
  end else begin
    fText := AText;
  end;

  TThread.Synchronize(nil,
    procedure()
    begin
      fWideChar := fText.toWideChar;
      try
        OutputDebugString(fWideChar);
      finally
        FreeMem(fWideChar);
      end;
    end);
end;

end.
