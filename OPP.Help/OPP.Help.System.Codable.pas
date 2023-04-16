unit OPP.Help.System.Codable;

interface

uses
  System.SysUtils, System.JSON,
  OPP.Help.Interfaces,
  OPP.Help.System.Setting.Editor.Defaults;

type
  OPPFileNotFoundException = class(Exception);
  OPPFileWriteException = class(Exception);
  OPPCoderRecreateException = class(Exception);
  OPPCoderDeserializeException = class(Exception);
  OPPCoderDecodeException = class(Exception);
  OPPCoderEncodeException = class(Exception);

  TOPPCoder<T> = class(TInterfacedObject, IOPPCodable<T>)
  private
    function Deserialize(data: TJSONObject): T;
  public
    procedure EncodeToJSON(const AFileName: String; const subject: T);
    procedure DecodeFromJSON(const AFileName: String; isUTF8: Boolean; out subject: T);
  end;

implementation

uses
  OPP.Help.System.Files,
  System.TypInfo,
  System.IOUtils, DBXJSONReflect, REST.JSON;

resourcestring
  SSFileNotFoundMessage = 'File not found';

  { TOPPHelpDefaultsCodable }

procedure TOPPCoder<T>.DecodeFromJSON(const AFileName: String; isUTF8: Boolean; out subject: T);
var
  fBuffer: System.TArray<System.Byte>;
  data: TJSONObject;
begin

  if not FileExists(AFileName) then
  begin
    raise OPPFileNotFoundException.Create(AFileName);
  end;

  fBuffer := TFile.ReadAllBytes(AFileName);
  try
    try
      data := TJSONObject.ParseJSONValue(fBuffer, 0, isUTF8) as TJSONObject;
      subject := Deserialize(data);
      if Assigned(data) then
        data.Free;
    except
      on OPPCoderDeserializeException do
      begin
        raise;
      end;
      on Exception do
      begin
        raise OPPCoderDecodeException.Create('');
      end;
    end;
  finally
    SetLength(fBuffer, 0);
  end;
end;

///
/// to cast interface follow https://en.delphipraxis.net/topic/4779-casting-an-object-as-a-interface-via-generic/
///
function TOPPCoder<T>.Deserialize(data: TJSONObject): T;
var
  fSerializer: TJSONUnMarshal;
  fResult: TObject;
begin

  fResult := nil;

  fSerializer := TJSONUnMarshal.Create;
  try
    try
      fResult := fSerializer.Unmarshal(data);

      if GetTypeKind(T) = tkClass then
      begin
        Result := T((@fResult)^);
      end;

    except
      on Exception do
      begin
        if Assigned(fResult) then
          FreeAndNil(fResult);
        raise OPPCoderDeserializeException.Create('');
      end;
    end;
  finally
    fSerializer.Free;
  end;
end;

procedure TOPPCoder<T>.EncodeToJSON(const AFileName: String; const subject: T);
var
  fFileName: String;
  fSerializer: TJSONMarshal;
  fJsonObject: TJSONObject;
  jsonString: String;
  fObjectToEncode: TObject;
  fSettingsPath: String;
begin

  fSerializer := TJSONMarshal.Create;
  try
    try
      fObjectToEncode := TObject((@subject)^);
      fJsonObject := fSerializer.marshal(fObjectToEncode) as TJSONObject;
    except
      on Error: Exception do
      begin
        raise OPPCoderEncodeException.Create(Error.message);
      end;
    end;

    if Assigned(fJsonObject) then
    begin
      try
        jsonString := TJson.JsonEncode(fJsonObject);
        try
          TFile.WriteAllText(AFileName, jsonString);
        except
          on Error: Exception do
          begin
            raise OPPFileWriteException.Create(Error.message);
          end;
        end;
      finally
        fJsonObject.Free;
      end;
    end;
  finally
    fSerializer.Free;
  end;
end;

end.
