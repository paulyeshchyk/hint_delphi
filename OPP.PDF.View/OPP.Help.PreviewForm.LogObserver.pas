unit OPP.Help.PreviewForm.LogObserver;

interface

uses
  System.Classes,
  OPP.Stream.Observer, cxMemo;

type
  TOPPHelpPreviewFormLogObserver = class(TInterfacedObject, IOPPStreamObserver)
  private
    fStream: TMemoryStream;
    fPeek: Int64;
    fcxMemo: TcxMemo;
    procedure WriteData(AData: UTF8String);
    procedure setCxMemo(const Value: TcxMemo);
  protected
    procedure StartListenStream(AStream: TStream);
    procedure StopListenStream(AStream: TStream);
    procedure WillChangeStream(AStream: TStream);
    procedure DidChangeStream(AStream: TStream);
  public
    constructor Create;
    destructor Destroy;override;
    property cxMemo: TcxMemo read fcxMemo write setCxMemo;
  end;

implementation

{ TOPPHelpPreviewFormLogObserver }

constructor TOPPHelpPreviewFormLogObserver.Create;
begin
  fStream := TMemoryStream.Create;
  fPeek := 0;
end;

destructor TOPPHelpPreviewFormLogObserver.Destroy;
begin
  fStream.Free;
  inherited;
end;

procedure TOPPHelpPreviewFormLogObserver.DidChangeStream(AStream: TStream);
var
  fUTF8String: UTF8String;
begin
  AStream.ReadWideChar(fPeek, fUTF8String);
  WriteData(fUTF8String);
end;

procedure TOPPHelpPreviewFormLogObserver.setCxMemo(const Value: TcxMemo);
begin
  fcxMemo := Value;
  if Assigned(fcxMemo) then begin
    fcxMemo.Lines.Clear;
    fcxMemo.Lines.LoadFromStream(fStream);
  end;
end;

procedure TOPPHelpPreviewFormLogObserver.StartListenStream(AStream: TStream);
begin
  fPeek := AStream.Size;
end;

procedure TOPPHelpPreviewFormLogObserver.StopListenStream(AStream: TStream);
begin
fPeek := 0;
end;

procedure TOPPHelpPreviewFormLogObserver.WillChangeStream(AStream: TStream);
begin
  fPeek := AStream.Position;
end;

procedure TOPPHelpPreviewFormLogObserver.WriteData(AData: UTF8String);
var
  fAnsiStr: String;
begin

  fAnsiStr := Utf8ToAnsi(AData);
  fStream.Write(fAnsiStr, Length(fAnsiStr));
  if assigned(cxMemo) then
    cxMemo.Lines.Add(fAnsiStr);
end;

end.
