unit OPP.Help.Log.Formatter;

interface

uses
  System.SysUtils,
  System.DateUtils;

type
  TOPPHelpLogDate = record
    sessionDate: TDateTime;
    messageDate: TDateTime;
  end;


  TOPPHelpLogFormatterDateTimeType = (dtNone, dtDateTime, dtElapsed);

  TOPPHelpDefaultLogFormatter = class abstract
  private
    fFlowName: String;
    fDateTimeFormatType: TOPPHelpLogFormatterDateTimeType;
    fForceLineBreak: Boolean;
    fDate: TOPPHelpLogDate;
  published
    class function isDebug: Boolean;virtual;
    constructor Create();
    function Format(AText: String): String; virtual;
    function IsDebugFormatter: Boolean; virtual;
    function GetFormatTemplate: String; virtual;
    property Date: TOPPHelpLogDate read fDate write fDate;
    property FormatTemplate: String read GetFormatTemplate;
    property FlowName: String read fFlowName write fFlowName;
    property ForceLineBreak: Boolean read fForceLineBreak write fForceLineBreak default false;
    property DateTimeFormatType: TOPPHelpLogFormatterDateTimeType read fDateTimeFormatType write fDateTimeFormatType default dtDateTime;
  end;

  TOPPHelpLogFormatterClass = class of TOPPHelpDefaultLogFormatter;

  TOPPHelpDebugLogFormatter = class(TOPPHelpDefaultLogFormatter)
  protected
    class function isDebug: Boolean;override;final;
    function GetFormatTemplate: String; override; final;
    function IsDebugFormatter: Boolean; override; final;
  end;

  TOPPHelpErrorLogFormatter = class(TOPPHelpDefaultLogFormatter)
  protected
    function GetFormatTemplate: String; override; final;
  end;

  TOPPHelpWarningLogFormatter = class(TOPPHelpDefaultLogFormatter)
  protected
    function GetFormatTemplate: String; override; final;
  end;

  TOPPHelpFlowLogFormatter = class(TOPPHelpDefaultLogFormatter)
  protected
    function GetFormatTemplate: String; override; final;
  end;

const
  kLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};


implementation

{ THelpLogFormatter }

constructor TOPPHelpDefaultLogFormatter.Create();
begin
  inherited Create;
  fFlowName := '';
  fDateTimeFormatType := dtDateTime;
end;

function TOPPHelpDefaultLogFormatter.Format(AText: String): String;
var
  fDateTimeStr: String;
  fFinalString: WideString;
  fFormatted: String;
  f: Double;
begin
  f := Double(System.DateUtils.MilliSecondsBetween(fDate.messageDate, fDate.sessionDate)) / 1000.0;
  case fDateTimeFormatType of
    dtDateTime: fDateTimeStr := FormatDateTime('YYYY.MM.DD hh:mm:ss:zzz', fDate.messageDate);
    dtElapsed : fDateTimeStr := System.SysUtils.Format('%6.3f',[f]);
    dtNone: fDateTimeStr := '';
  end;


  fFinalString := System.SysUtils.Format(self.FormatTemplate, [fFlowName, AText]);
  if fForceLineBreak then
    result := System.SysUtils.Format('[%s] %s%s', [fDateTimeStr, fFinalString, kLineBreak])
  else
    result := System.SysUtils.Format('[%s] %s', [fDateTimeStr, fFinalString]);
end;

function TOPPHelpDefaultLogFormatter.GetFormatTemplate: String;
begin
  result := '[??]: %s %s';
end;

class function TOPPHelpDefaultLogFormatter.isDebug: Boolean;
begin
  result := false;
end;

function TOPPHelpDefaultLogFormatter.IsDebugFormatter: Boolean;
begin
  result := false;
end;

{ TDebugLogFormatter }
function TOPPHelpDebugLogFormatter.GetFormatTemplate: String;
begin
  result := '[Debug]: %s %s';
end;

class function TOPPHelpDebugLogFormatter.isDebug: Boolean;
begin
  result := true;
end;

function TOPPHelpDebugLogFormatter.IsDebugFormatter: Boolean;
begin
  result := true;
end;

{ TErrorLogFormatter }

function TOPPHelpErrorLogFormatter.GetFormatTemplate: String;
begin
  result := '[Error]: %s %s';
end;

{ TWarningLogFormatter }

function TOPPHelpWarningLogFormatter.GetFormatTemplate: String;
begin
  result := '[Warning]: %s %s';
end;

{ TFlowLogFormatter }

function TOPPHelpFlowLogFormatter.GetFormatTemplate: String;
begin
  result := '[%s]: %s';
end;

end.
