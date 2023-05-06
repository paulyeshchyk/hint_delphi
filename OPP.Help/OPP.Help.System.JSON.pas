unit OPP.Help.System.JSON;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
  Rest.JSON,
  //Data.DBXJSONReflect,
  System.IOUtils,

  Vcl.Dialogs;

type
  TOPPJSONParserCallback<T: class, constructor> = reference to procedure(Mapset: T; Error: Exception);
  TOPPJSONWriteCompletion = reference to procedure(error: Exception);

  TOPPJSONParser = class
  private
    class procedure DeserializeJSON<T: class, constructor>(AJSON: TJSONObject; callback: TOPPJSONParserCallback<T>);
  public
    class function Serialize<T: class, constructor>(AObject: T): String;overload;
    class function Serialize<T: class, constructor>(AObject: T; AFilename: String; callback: TOPPJSONWriteCompletion): Integer;overload;
    class procedure Deserialize<T: class, constructor>(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPJSONParserCallback<T>);overload;
    class procedure Deserialize<T: class, constructor>(AFileName: String; callback: TOPPJSONParserCallback<T>);overload;
  end;

implementation

uses
  OPP.Help.Log, OPP.Help.System.Files;

class procedure TOPPJSONParser.deserializeJSON<T>(AJSON: TJSONObject; callback: TOPPJSONParserCallback<T>);
var
  fResult: T;
  Error: Exception;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
    begin
      Error := Exception.Create('JSON is not assigned');
      try
        callback(Default (T), Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  try
    fResult := TJson.JsonToObject<T>(AJSON);
    try
      if assigned(callback) then
      begin
        callback(fResult, nil);
      end;
    finally
      FreeAndNil(fResult);
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
      if assigned(callback) then
        callback(nil, E);
    end;
  end;

end;

class function TOPPJSONParser.Serialize<T>(AObject: T): String;
begin
  result := TJson.ObjectToJsonString(AObject);
end;

class procedure TOPPJSONParser.deserialize<T>(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPJSONParserCallback<T>);
var
  jsonObject: TJSONObject;
begin
  jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
  try
    try
      TOPPJSONParser.deserializeJSON<T>(jsonObject, callback);
    except
      on E: Exception do
      begin
        eventLogger.Error(E);
        if assigned(callback) then
        begin
          callback(nil, E);
        end;
      end;
    end;
  finally
    FreeAndNil(jsonObject);
  end;
end;


class procedure TOPPJSONParser.deserialize<T>(AFileName: String; callback: TOPPJSONParserCallback<T>);
var
  bytes: System.TArray<System.Byte>;
  Error: Exception;
begin
  if not FileExists(AFileName) then
  begin
    if assigned(callback) then
    begin
      Error := Exception.Create(Format('File not found: %s', [AFileName]));
      try
        callback(nil, Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  try
    bytes := TFile.ReadAllBytes(AFileName);
    try
      deserialize<T>(bytes, false, callback);
    finally
      SetLength(bytes, 0);
    end;
  except
    on Error: Exception do
    begin
      eventLogger.Error(Format('Error: %s; %s', [Error.Message, TOPPHelpSystemFilesHelper.AbsolutePath(AFileName)]));
    end;
  end;
end;


class function TOPPJSONParser.Serialize<T>(AObject: T; AFilename: String; callback: TOPPJSONWriteCompletion): Integer;
var
  jsonObj: TJSONObject;
  jsonString: String;

begin
  result := -1;

  if not TOPPHelpSystemFilesHelper.CreateDirectoryIfNeed(AFileName) then begin
    exit;
  end;

  jsonString := TOPPJSONParser.Serialize<T>(AObject);
  try
    TFile.WriteAllText(AFileName, jsonString);
    if assigned(callback) then
      callback(nil);
    result := 0;
  except
    on Error: Exception do
    begin
      eventLogger.Error(Error);
      if assigned(callback) then
        callback(Error);
    end;
  end;

end;

end.
