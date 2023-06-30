unit OPP.Stream.Observer;

interface

uses
  System.Classes, System.SysUtils;

type

  IOPPStreamObserver = interface
    procedure StartListenStream(AStream: TStream);
    procedure StopListenStream(AStream: TStream);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
  end;

  IOPPStreamable = interface
    procedure RegisterObserver(AObserver: IOPPStreamObserver);
    procedure UnregisterObserver(AObserver: IOPPStreamObserver);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
    function GetStream: TStream;
  end;

  TOPPStreamHelper = class helper for TStream
  private
  public
    procedure WriteString(const AString: UTF8String);
    procedure ReadWideChar(APosition: Int64; out AUTF8String: UTF8String);
  end;

implementation

{ TOPPStreamHelper }

procedure TOPPStreamHelper.ReadWideChar(APosition: Int64; out AUTF8String: UTF8String);
var
  fLength: Integer;
  fPosition: Integer;
  fBuffer: System.TArray<Byte>;
  fResult: String;
  LByteOrderMark: TBytes;
  LOffset: Integer;
  LBuffer: TBytes;
  LEncoding, DestEncoding: TEncoding;
  str: String;
  bytes: TArray<Byte>;
begin
  LEncoding := nil;
  DestEncoding := TEncoding.UTF8;
  fPosition := self.Position;
  fLength := (self.Size - APosition);
  self.Position := APosition;
  SetLength(LBuffer, fLength);
  self.read(Pointer(LBuffer)^, fLength);
  LOffset := TEncoding.GetBufferEncoding(LBuffer, LEncoding);
  AUTF8String := DestEncoding.GetString(LBuffer, LOffset, Length(LBuffer) - LOffset);
end;

procedure TOPPStreamHelper.WriteString(const AString: UTF8String);
var
  fLength: Integer;
  LEncoding: TEncoding;
  LByteOrderMark: TBytes;
  bytes: TArray<Byte>;
begin
  LEncoding := TEncoding.UTF8;
  LByteOrderMark := LEncoding.GetPreamble;
  bytes := TEncoding.UTF8.GetBytes(AString);
  self.Write(LByteOrderMark[0], Length(LByteOrderMark));
  self.Write(bytes[0], Length(bytes));
end;

end.
