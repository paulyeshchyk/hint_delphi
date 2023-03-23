unit OPP.Help.System.Stream;

interface

uses
  System.Classes, System.SysUtils;

type
  TStreamHelper = class helper for TStream
  public
    function ReadInteger: Integer;
    function ReadDouble: Double;
    function ReadString: String;
    procedure WriteInteger(Value: Integer);
    procedure WriteDouble(Value: Double);
    procedure WriteString(const Value: String);
  end;

  TReadOnlyMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(APtr: Pointer; ASize: NativeInt);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

constructor TReadOnlyMemoryStream.Create(APtr: Pointer; ASize: NativeInt);
begin
  inherited Create;
  SetPointer(APtr, ASize);
end;

function TReadOnlyMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

{ --------------------------- }
function TStreamHelper.ReadInteger: Integer;
begin
  ReadBuffer(Result, SizeOf(Integer));
end;

function TStreamHelper.ReadDouble: Double;
begin
  ReadBuffer(Result, SizeOf(Double));
end;

function TStreamHelper.ReadString: String;
var
  fBytes: TBytes;
  fLength: Integer;
begin
  fLength := ReadInteger;
  SetLength(fBytes, fLength);
  Self.ReadBuffer(PByte(fBytes)^, fLength);
  Result := TEncoding.UTF8.GetString(fBytes);
end;

procedure TStreamHelper.WriteInteger(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteDouble(Value: Double);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteString(const Value: String);
var
  fBytes: TBytes;
  fLength: Integer;
begin
  fBytes := TEncoding.UTF8.GetBytes(Value);
  fLength := Length(fBytes);
  WriteInteger(fLength);
  Self.WriteBuffer(PByte(fBytes)^, fLength);
end;

end.
