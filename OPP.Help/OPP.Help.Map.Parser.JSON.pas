unit OPP.Help.Map.Parser.JSON;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,

  OPP.Help.System.References,
  OPP.Help.Map;

type

  TOPPHelpMapRESTParserExtended = class
  private
    class procedure parseExtendedJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpMapParserJSONCallback = nil);
    class procedure deserializeExtendedJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
  public
    class procedure readExtendedJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
    class function saveExtendedJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
  end;

  TOPPHelpMapRESTParser = class
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

class procedure TOPPHelpMapRESTParserExtended.deserializeExtendedJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
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
  finally
    deSerializer.Free;
  end;
end;

class procedure TOPPHelpMapRESTParser.deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
var
  mapList: TOPPHelpMapSet;
  //deSerializer: TJSONUnMarshal;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
      callback(nil, Exception.Create('JSON is not assigned'));
    exit;
  end;

  try
    mapList := TJson.JsonToObject<TOPPHelpMapSet>(AJSON);
    if assigned(callback) then
    begin
      callback(mapList.list, nil);
    end;
    //FreeAndNil(deSerializer);
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

class procedure TOPPHelpMapRESTParserExtended.parseExtendedJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapParserJSONCallback);
var
  jsonObject: TJSONObject;
begin
  try
    jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
    deserializeExtendedJSON(jsonObject, callback);
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

class procedure TOPPHelpMapRESTParser.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapParserJSONCallback);
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

class procedure TOPPHelpMapRESTParserExtended.readExtendedJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
begin

end;

class procedure TOPPHelpMapRESTParser.readJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
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

class function TOPPHelpMapRESTParserExtended.saveExtendedJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
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

class function TOPPHelpMapRESTParser.saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  //

  jsonString := TJson.ObjectToJsonString(TOPPHelpMapSet.Create(AList));
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

  result := 0;
end;

end.
