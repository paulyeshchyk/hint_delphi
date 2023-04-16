unit OPP.Help.System.Codable.Helper;

interface

uses
  System.SysUtils,
  OPP.Help.System.Codable;

type

  TOPPJsonDecodeException = class(Exception);
  TOPPJsonEncodeException = class(Exception);

  TOPPCodableHelper<T: TOPPCodable> = class
  private
    class function CreateDefaults(AFileName: String): T;
    class function ReCreateDefaults(AFileName: String): T;
    class procedure SetDefaultsAndSave(AValue: T; AFileName: String);
  public
    class procedure Decode(AFileName: String; out AValue: T);
    class procedure Encode(AFileName: String; AValue: T);
  end;

implementation

uses OPP.Help.System.Files, OPP.Help.Log;

class function TOPPCodableHelper<T>.CreateDefaults(AFileName: String): T;
begin
  result := T.Create();
  try
    SetDefaultsAndSave(result, AFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

class function TOPPCodableHelper<T>.ReCreateDefaults(AFileName: String): T;
begin
  result := T.Create();
  try
    SetDefaultsAndSave(result, AFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
      if assigned(result) then
        FreeAndNil(result);
      raise OPPCoderRecreateException.Create('');
    end;
  end;
end;

class procedure TOPPCodableHelper<T>.SetDefaultsAndSave(AValue: T; AFileName: String);
begin
  if not assigned(AValue) then
    exit;

  AValue.SetDefaults();
  Self.Encode(AFileName, AValue);
end;

class procedure TOPPCodableHelper<T>.Decode(AFileName: String; out AValue: T);
var
  fOPPDecoder: TOPPCoder<T>;
  fFileName: String;
begin

  AValue := nil;

  fFileName := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFileName);

  fOPPDecoder := TOPPCoder<T>.Create;
  try
    try
      fOPPDecoder.DecodeFromJSON(fFileName, true, AValue);
    except
      on E: OPPFileNotFoundException do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        AValue := CreateDefaults(AFileName);
      end;
      on E: OPPCoderDeserializeException do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        AValue := ReCreateDefaults(AFileName);
      end;
      on E: Exception do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        raise TOPPJsonDecodeException.Create('');
      end;
    end;
  finally
    fOPPDecoder.Free;
  end;
end;

class procedure TOPPCodableHelper<T>.Encode(AFileName: String; AValue: T);
var
  fOPPDecoder: TOPPCoder<T>;
  fFileName: String;
begin

  fFileName := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFileName);

  fOPPDecoder := TOPPCoder<T>.Create;
  try
    try
      fOPPDecoder.EncodeToJSON(fFileName, AValue);
    except
      on E: Exception do
      begin
        eventLogger.Error(E);
        raise TOPPJsonEncodeException.Create('');
      end;
    end;
  finally
    fOPPDecoder.Free;
  end;
end;

end.
