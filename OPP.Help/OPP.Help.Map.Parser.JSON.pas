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
  DBXJSONReflect, REST.JSON, System.IOUtils,
  Vcl.Dialogs;

class procedure TOPPHelpMapRESTParserExtended.deserializeExtendedJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
var
  mapList: TOPPHelpMapSet;
  deSerializer: TJSONUnMarshal;
  Error: Exception;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
    begin
      Error := Exception.Create('JSON is not assigned');
      try
        callback(nil, Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  deSerializer := TJSONUnMarshal.Create;
  try
    try
      mapList := deSerializer.unmarshal(AJSON) as TOPPHelpMapSet;
      try
        if assigned(callback) then
        begin
          callback(mapList, nil);
        end;
      finally
        mapList.Free;
      end;
      FreeAndNil(deSerializer);
    except
      on E: Exception do
      begin
        if assigned(callback) then
          callback(nil, E)
        else
          eventLogger.Error(E);
      end;
    end;
  finally
    deSerializer.Free;
  end;
end;

class procedure TOPPHelpMapRESTParser.deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpMapParserJSONCallback);
var
  mapList: TOPPHelpMapSet;
  Error: Exception;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
    begin
      Error := Exception.Create('JSON is not assigned');
      try
        callback(nil, Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  try
    mapList := TJson.JsonToObject<TOPPHelpMapSet>(AJSON);
    try
      if assigned(callback) then
      begin
        callback(mapList, nil);
      end;
    finally
      mapList.list.Clear;
      mapList.list.Pack;
      FreeAndNil(mapList);
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

class procedure TOPPHelpMapRESTParserExtended.parseExtendedJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapParserJSONCallback);
var
  jsonObject: TJSONObject;
begin
  jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
  try
    try
      deserializeExtendedJSON(jsonObject, callback);
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
    jsonObject.Free;
  end;
end;

class procedure TOPPHelpMapRESTParser.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapParserJSONCallback);
var
  jsonObject: TJSONObject;
begin
  jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
  try
    try
      DeserializeJSON(jsonObject, callback);
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

class procedure TOPPHelpMapRESTParserExtended.readExtendedJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
begin

end;

class procedure TOPPHelpMapRESTParser.readJSON(AFileName: String; callback: TOPPHelpMapParserJSONCallback);
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
      parseJSONBytes(bytes, true, callback);
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

class function TOPPHelpMapRESTParserExtended.saveExtendedJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  serializer := TJSONMarshal.Create;
  try
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
          eventLogger.Error(Error);
          if assigned(callback) then
            callback(Error);
        end;
      end;
    finally
      jsonObj.Free;
    end;
  finally
    FreeAndNil(serializer);
  end;

  result := 0;

end;

class function TOPPHelpMapRESTParser.saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;

  function createDirectoryIfNeed(AFileName: String): Boolean;
  var
    fDirectoryPath: String;
  begin
    result := false;
    if Length(AFileName) = 0 then begin
      eventLogger.Error('Path is empty');
      exit;
    end;

    fDirectoryPath := System.SysUtils.ExtractFilePath(AFileName);

    if System.IOUtils.TDirectory.Exists(fDirectoryPath) then
    begin
      result := true;
      exit;
    end;

    try
      System.IOUtils.TDirectory.CreateDirectory(fDirectoryPath);
      result := true;
    except
      on e: Exception do
      begin
        eventLogger.Error(e);
        ShowMessage(Format('Not able to save file: %s, because %s',[AFileName, e.Message]));
        result := false;
      end;
    end;
  end;

begin
  //

  if not createDirectoryIfNeed(AFileName) then begin
    exit;
  end;

  jsonString := TJson.ObjectToJsonString(TOPPHelpMapSet.Create(AList));
  try
    TFile.WriteAllText(AFileName, jsonString);
    if assigned(callback) then
      callback(nil);
  except
    on Error: Exception do
    begin
      eventLogger.Error(Error);
      if assigned(callback) then
        callback(Error);
    end;
  end;

  result := 0;
end;

end.
