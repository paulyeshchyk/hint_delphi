unit OPP.Help.Log.OutputFile;

interface

uses
  System.IOUtils,
  System.Classes,
  System.SysUtils,
  OPP.Help.Log.Formatter,
  OPP.Help.Log.Output,
  OPP.Help.System.Str,
  OPP.Help.System.Files;

type
  TOPPHelpLogOutputFile = class(TInterfacedObject, IOPPHelpLogOutput)
  private
    fFileStream: TFileStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartSession(date: TDateTime);
    procedure EndSession(date: TDateTime);
    procedure WriteText(const date: TOPPHelpLogDate; const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
  end;

implementation

uses
  WinAPI.Windows;

{ TOPPFileLogOutput }

constructor TOPPHelpLogOutputFile.Create;
begin
  inherited Create;
  //
end;

destructor TOPPHelpLogOutputFile.Destroy;
begin
  //
  inherited;
end;

procedure TOPPHelpLogOutputFile.EndSession(date: TDateTime);
begin
  //
end;

procedure TOPPHelpLogOutputFile.StartSession(date: TDateTime);
var
  fDirectory: String;
  fFileName: String;
  fDateTimeTemplate: String;
begin
  if assigned(fFileStream) then
  begin
    fFileStream.Free;
  end;

  fDateTimeTemplate := FormatDateTime('YYYY-MM-DD_hh-mm-ss', date);
  fFileName := TOPPHelpSystemFilesHelper.GetOPPLogsPath(Format('%s.log', [fDateTimeTemplate]));
  try
    fFileStream := TFile.Create(fFileName);
  except
    on E: Exception do
    begin
      fFileStream := nil;
      OutputDebugString(E.message.toWideChar);
    end;
  end;
end;

procedure TOPPHelpLogOutputFile.WriteText(const date: TOPPHelpLogDate; const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
var
  fLength: Int64;
  fText: String;
  fFormatter: TOPPHelpDefaultLogFormatter;
begin
  if not assigned(fFileStream) then
    exit;

  if not Assigned(AFormatterClass) then
    exit;

  fFormatter := AFormatterClass.Create();
  try
    fFormatter.FlowName := AFlowName;
    fFormatter.DateTimeFormatType := dtElapsed;
    fFormatter.Date := date;
    fFormatter.ForceLineBreak := true;
    fText := fFormatter.Format(AText);
  finally
    fFormatter.Free;
  end;

  fLength := Length(fText);
  try
    fFileStream.WriteBuffer(fText[1], SizeOf(fText[1]) * fLength);
  except
    on E: Exception do
    begin
      OutputDebugString(E.message.toWideChar);
    end;
  end;
end;

end.

