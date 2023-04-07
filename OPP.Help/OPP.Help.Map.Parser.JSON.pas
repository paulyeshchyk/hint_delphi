unit OPP.Help.Map.Parser.JSON;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
  OPP.Help.nonatomic,
  OPP.Help.Map;

type
  TOPPHelpMapParserJSONCallback = reference to procedure(AList: TList<TOPPHelpMap>; Error: Exception);

  TOPPHelpMapParserJSON = class helper for TOPPHelpMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpMapParserJSONCallback = nil);
    class procedure deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
    class function saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
  end;

implementation

uses
  OPP.Help.Log, OPP.Help.System.Error, OPP.Help.System.Files,
  DBXJSONReflect, REST.JSON, System.IOUtils;

class procedure TOPPHelpMapParserJSON.deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
var
  mapList: TOPPHelpMapSet;
  deSerializer: TJSONUnMarshal;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
      callback(nil, Exception.Create('JSON is not assigned'));
    exit;
  end;

  deSerializer := TJSONUnMarshal.Create;
  try
    mapList := deSerializer.unmarshal(AJSON) as TOPPHelpMapSet;
    if assigned(callback) then
    begin
      callback(mapList.list, nil);
    end;
    FreeAndNil(deSerializer);
  except
    on E: Exception do
    begin
      if assigned(callback) then
        callback(nil, E)
      else
        E.Log();
    end;
  end;
end;

class procedure TOPPHelpMapParserJSON.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapParserJSONCallback);
var
  jsonObject: TJSONObject;
begin
  try
    jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
    deserializeJSON(jsonObject, callback);
    FreeAndNil(jsonObject);
  except
    on E: Exception do
    begin
      E.Log();
      if assigned(callback) then
      begin
        callback(nil, E);
      end;
    end;
  end;
end;

class procedure TOPPHelpMapParserJSON.readJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
var
  bytes: System.TArray<System.Byte>;
begin
  if not FileExists(AFileName) then
  begin
    if assigned(callback) then
      callback(nil, Exception.Create(Format('File not found: %s', [AFileName])));
    exit;
  end;

  try
    bytes := TFile.ReadAllBytes(AFileName);
    parseJSONBytes(bytes, true, callback);
  except
    on Error: Exception do
    begin
      Error.Log(TOPPHelpSystemFilesHelper.AbsolutePath(AFileName));
    end;
  end;
end;

class function TOPPHelpMapParserJSON.saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  //
  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(TOPPHelpMapSet.Create(AList)) as TJSONObject;
  try
    jsonString := TJson.JsonEncode(jsonObj);
    try
      TFile.WriteAllText(AFileName, jsonString);
      if assigned(callback) then
        callback(nil);
    except
      on Error: Exception do
      begin
        Error.Log();
        if assigned(callback) then
          callback(Error);
      end;
    end;
  finally
    jsonObj.Free;
    FreeAndNil(serializer);
  end;

  result := 0;
end;

end.
