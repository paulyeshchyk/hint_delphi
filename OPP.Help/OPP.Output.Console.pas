unit OPP.Output.Console;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  OPP.Help.System.Files,
  OPP.Stream.Observer;

type

  TOPPFileOutput = class(TInterfacedObject, IOPPStreamObserver)
  private
    fPeek: Int64;
    fFileStream: TFileStream;
    procedure startSession;
    procedure endSession;
    procedure WriteData(AData: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
  protected
    procedure StartListenStream(AStream: TStream);
    procedure StopListenStream(AStream: TStream);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
  end;

  TOPPConsoleOutput = class(TInterfacedObject, IOPPStreamObserver)
  private
    fPeek: Int64;
  public
    constructor Create;
    destructor Destroy; override;
  protected
    procedure StartListenStream(AStream: TStream);
    procedure StopListenStream(AStream: TStream);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
  end;

implementation

uses
  OPP.Help.System.Str,
  Vcl.Forms,
  WinAPI.Windows;

{ TOPPConsoleOutput }

constructor TOPPConsoleOutput.Create;
begin
  fPeek := 0;
end;

destructor TOPPConsoleOutput.Destroy;
begin
  //
  inherited;
end;

procedure TOPPConsoleOutput.DidChangeStream(AStream: TStream);
var
  fUTF8String: UTF8String;
  fAnsiStr: String;
begin

  AStream.ReadWideChar(fPeek, fUTF8String);

  fAnsiStr := Utf8ToAnsi(fUTF8String);

  OutputDebugString(fAnsiStr.towideChar);
end;

procedure TOPPConsoleOutput.StartListenStream(AStream: TStream);
begin
  fPeek := AStream.Size;
end;

procedure TOPPConsoleOutput.StopListenStream(AStream: TStream);
begin
  fPeek := 0;
end;

procedure TOPPConsoleOutput.WillChangeStream(AStream: TStream);
begin
  fPeek := AStream.Position;
end;

{ TOPPFileOutput }

constructor TOPPFileOutput.Create;
var
  fMode: Word;
  fFileName: String;
begin
  fMode := fmOpenReadWrite or fmShareDenyNone;
  fFileName := TOPPHelpSystemFilesHelper.GetOPPLogsPath(Format('%s.log', [ExtractFileName(Application.ExeName)]));
  if not TFile.Exists(fFileName) then
    fMode := fmCreate or fmShareDenyNone;

  fPeek := 0;
  fFileStream := TFileStream.Create(fFileName, fMode);
  fFileStream.Seek(0, TSeekOrigin.soEnd);

  startSession;
end;

destructor TOPPFileOutput.Destroy;
begin
  endSession;
  fFileStream.Free;
  inherited;
end;

procedure TOPPFileOutput.DidChangeStream(AStream: TStream);
var
  fData: UTF8String;
begin
  AStream.ReadWideChar(fPeek, fData);

  WriteData(Format('[%s]: %s',[FormatDateTime('YYYY-MM-DD hh:mm:ss:zzz', now),fData]));
end;

procedure TOPPFileOutput.endSession;
begin
  WriteData(Format('[%s]: --- end session ---', [FormatDateTime('YYYY-MM-DD hh:mm:ss:zzz', now)]))
end;

procedure TOPPFileOutput.StartListenStream(AStream: TStream);
begin
  fPeek := AStream.Size;
end;

procedure TOPPFileOutput.startSession;
begin
  WriteData(Format('[%s]: --- start session ---', [FormatDateTime('YYYY-MM-DD hh:mm:ss:zzz', now)]))
end;

procedure TOPPFileOutput.StopListenStream(AStream: TStream);
begin
  fPeek := 0;
end;

procedure TOPPFileOutput.WillChangeStream(AStream: TStream);
begin
  fPeek := AStream.Position;
end;

procedure TOPPFileOutput.WriteData(AData: UTF8String);
var
  len: Int64;
  fAnsiStr: String;
const
  eol: String = #13#10;
begin
  fAnsiStr := Utf8ToAnsi(AData);
  len := Length(fAnsiStr);
  if len > 0 then
  begin
    fFileStream.WriteBuffer(fAnsiStr[1], len * SizeOf(fAnsiStr[1]));
    len := Length(eol);
    fFileStream.WriteBuffer(eol[1], len * SizeOf(eol[1]));
  end;
end;

end.
