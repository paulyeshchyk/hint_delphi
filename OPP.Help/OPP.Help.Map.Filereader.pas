unit OPP.Help.Map.Filereader;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  OPP.Help.Nonatomic,
  OPP.Help.Map;

type
  TOPPHelpHintMapJSONReadCallback = reference to procedure(AList: TList<TOPPHelpMap>; error: Exception);

  TOPPHelpHintMapFileReader = class helper for TOPPHelpMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpHintMapJSONReadCallback = nil);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpHintMapJSONReadCallback);
    class function saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
  end;

implementation

uses
  OPP.Help.System.error, OPP.Help.Log,

  System.JSON, System.IOUtils,
  DBXJSONReflect, REST.JSON;

class procedure TOPPHelpHintMapFileReader.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpHintMapJSONReadCallback);
var
  deSerializer: TJSONUnMarshal;
  jsonObject: TJSONObject;
  mapList: TOPPHelpMapSet;
  list: TList<TOPPHelpMap>;
  error: Exception;
begin
  error := nil;
  deSerializer := TJSONUnMarshal.Create;
  list := TList<TOPPHelpMap>.Create;
  try
    try
      jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
      try
        mapList := deSerializer.unmarshal(jsonObject) as TOPPHelpMapSet;
        list.AddRange(mapList.list);
        FreeAndNil(mapList);
      except
        on E: Exception do
        begin
          E.Log();
          error := E;
        end;
      end;
    except
      on E: Exception do
      begin
        E.Log();
        error := E;
      end;
    end;
  finally
    if assigned(callback) then
    begin
      callback(list, error);
    end;
    FreeAndNil(jsonObject);
    FreeAndNil(list);
    FreeAndNil(deSerializer);
  end;
end;

class procedure TOPPHelpHintMapFileReader.readJSON(AFileName: String; callback: TOPPHelpHintMapJSONReadCallback);
var
  bytes: System.TArray<System.Byte>;
  error: Exception;
begin

  if not FileExists(AFileName) then
  begin
    error := Exception.Create(Format('File not found: %s', [AFileName]));
    try
      if assigned(callback) then
        callback(nil, error)
      else
        error.Log();
    finally
      error.Free;
    end;

    exit;
  end;

  try
    try
      bytes := TFile.ReadAllBytes(AFileName);
    except
      on E: Exception do
      begin
        E.Log();
      end;

    end;
  finally
    parseJSONBytes(bytes, true, callback);
  end;
end;

class function TOPPHelpHintMapFileReader.saveJSON(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  result := 0;

  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(TOPPHelpMapSet.Create(AList)) as TJSONObject;
  try
    jsonString := TJson.JsonEncode(jsonObj);
    try
      TFile.WriteAllText(AFileName, jsonString);
      if assigned(callback) then
        callback(nil);
    except
      on E: Exception do
      begin
        result := -1;
        if assigned(callback) then
          callback(E);
        E.Log;
      end;
    end;
  finally
    jsonObj.Free;
    FreeAndNil(serializer);
  end;
end;

end.
