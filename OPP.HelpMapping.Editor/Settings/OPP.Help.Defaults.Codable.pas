unit OPP.Help.Defaults.Codable;

interface

uses
  System.SysUtils, System.JSON,
  OPP.Help.Interfaces, OPP.Help.Defaults;

type
  OPPFileNotFoundException = class(Exception);

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
  fFileName: String;
  data: TJSONObject;
begin
  fFileName := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
  if not FileExists(fFileName) then
  begin
    raise OPPFileNotFoundException.Create(AFileName);
  end;

  fBuffer := TFile.ReadAllBytes(fFileName);
  try
    try
      data := TJSONObject.ParseJSONValue(fBuffer, 0, isUTF8) as TJSONObject;
      subject := Deserialize(data);
      if Assigned(data) then
        data.Free;
    except
      on Error: Exception do
      begin
        raise;
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
  fSerializer := TJSONUnMarshal.Create;
  try
    try
      fResult := fSerializer.Unmarshal(data);

      if GetTypeKind(T) = tkClass then
      begin
        Result := T((@fResult)^);
      end;

    except
      on Error: Exception do
      begin
        raise;
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
  jsonObj: TJSONObject;
  jsonString: String;
  fObjectToEncode: TObject;
begin

  fSerializer := TJSONMarshal.Create;
  try
    try
      fObjectToEncode := TObject((@subject)^);
      jsonObj := fSerializer.marshal(fObjectToEncode) as TJSONObject;
    except
      on Error: Exception do
      begin
        raise;
      end;
    end;
    try
      jsonString := TJson.JsonEncode(jsonObj);
      try
        fFileName := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
        TFile.WriteAllText(fFileName, jsonString);
      except
        on Error: Exception do
        begin
          raise;
        end;
      end;
    finally
      jsonObj.Free;
    end;
  finally
    fSerializer.Free;
  end;
end;

end.
