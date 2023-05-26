unit OPP.Help.System.JSON;

interface

uses
  System.Generics.Collections, System.Classes,
  System.SysUtils,
  System.JSON,
  Rest.JSON,
  System.IOUtils,

  Vcl.Dialogs;

type
  TOPPJSONParserCallback<T: class, constructor> = reference to procedure(Mapset: T; Error: Exception);
  TOPPJSONWriteCompletion = reference to procedure(Error: Exception);

  TOPPJSONParser = class
  private
    class procedure DeserializeJSON<T: class, constructor>(AJSON: TJSONObject; callback: TOPPJSONParserCallback<T>);
  public
    class function Serialize<T: class, constructor>(AObject: T): String; overload;
    class function Serialize<T: class, constructor>(AObject: T; AFilename: String; callback: TOPPJSONWriteCompletion): Integer; overload;
    class procedure DeSerialize<T: class, constructor>(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPJSONParserCallback<T>); overload;
    class procedure DeSerialize<T: class, constructor>(AFilename: String; callback: TOPPJSONParserCallback<T>); overload;
  end;

implementation

uses
  OPP.Help.Log, OPP.Help.System.Files;

class procedure TOPPJSONParser.DeserializeJSON<T>(AJSON: TJSONObject; callback: TOPPJSONParserCallback<T>);
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
      eventLogger.Error(E, 'TOPPJSONParser');
      if assigned(callback) then
        callback(nil, E);
    end;
  end;

end;

class function TOPPJSONParser.Serialize<T>(AObject: T): String;
begin
  try
    result := TJson.ObjectToJsonString(AObject);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'TOPPJSONParser');
    end;
  end;
end;

class procedure TOPPJSONParser.DeSerialize<T>(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPJSONParserCallback<T>);
var
  jsonObject: TJSONObject;
begin
  jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
  if not assigned(jsonObject) then
  begin
    eventLogger.Warning('jsonObject is not defined', 'TOPPJSONParser');
    if assigned(callback) then
      callback(nil, nil);
    exit;
  end;
  try
    TOPPJSONParser.DeserializeJSON<T>(jsonObject, callback);
  finally
    FreeAndNil(jsonObject);
  end;
end;

class procedure TOPPJSONParser.DeSerialize<T>(AFilename: String; callback: TOPPJSONParserCallback<T>);
var
  bytes: System.TArray<System.Byte>;
  Error: Exception;
begin
  if not FileExists(AFilename) then
  begin
    if assigned(callback) then
    begin
      Error := Exception.Create(Format('File not found: %s', [AFilename]));
      try
        callback(nil, Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  try
    bytes := TFile.ReadAllBytes(AFilename);
    try
      DeSerialize<T>(bytes, false, callback);
    finally
      SetLength(bytes, 0);
    end;
  except
    on Error: Exception do
    begin
      eventLogger.Error(Format('Error: %s; %s', [Error.Message, TOPPHelpSystemFilesHelper.AbsolutePath(AFilename)]));
    end;
  end;
end;

class function TOPPJSONParser.Serialize<T>(AObject: T; AFilename: String; callback: TOPPJSONWriteCompletion): Integer;
var
  jsonObj: TJSONObject;
  jsonString: String;

begin
  result := -1;

  if not TOPPHelpSystemFilesHelper.CreateDirectoryIfNeed(AFilename) then
  begin
    exit;
  end;

  jsonString := TOPPJSONParser.Serialize<T>(AObject);
  try
    TFile.WriteAllText(AFilename, jsonString);
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
