unit OPP.Help.Shortcut.Mapping.Filereader;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON, System.IOUtils,
  OPP.Help.Map, OPP.Help.System.Error, OPP.Help.System.Files;

type
  TOPPHelpShortcutMapJSONReadCallback = reference to procedure(AList: TList<TOPPHelpMap>);

  TOPPHelpShortcutMapFileReader = class helper for TOPPHelpMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpShortcutMapJSONReadCallback = nil);
    class procedure deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpShortcutMapJSONReadCallback);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpShortcutMapJSONReadCallback);
    class function saveJSON(AList: TList<TOPPHelpMap>; AFileName: String): Integer;
  end;

implementation

uses
  DBXJSONReflect, REST.JSON;

class procedure TOPPHelpShortcutMapFileReader.deserializeJSON(AJSON: TJSONObject; callback: TOPPHelpShortcutMapJSONReadCallback);
var
  mapList: TOPPHelpMapSet;
  deSerializer: TJSONUnMarshal;
begin
  if not assigned(AJSON) then
  begin
    if assigned(callback) then
      callback(nil);
    exit;
  end;

  deSerializer := TJSONUnMarshal.Create;
  try
    mapList := deSerializer.unmarshal(AJSON) as TOPPHelpMapSet;
    if assigned(callback) then
    begin
      callback(mapList.list);
    end;
    FreeAndNil(deSerializer);
  except
    on E: Exception do
    begin
      E.Log();
      if assigned(callback) then
      begin
        callback(nil);
      end;
    end;
  end;
end;

class procedure TOPPHelpShortcutMapFileReader.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpShortcutMapJSONReadCallback);
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
        callback(nil);
      end;
    end;
  end;
end;

class procedure TOPPHelpShortcutMapFileReader.readJSON(AFileName: String; callback: TOPPHelpShortcutMapJSONReadCallback);
var
  bytes: System.TArray<System.Byte>;
begin
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

class function TOPPHelpShortcutMapFileReader.saveJSON(AList: TList<TOPPHelpMap>; AFileName: String): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  //
  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(TOPPHelpMapSet.Create(AList)) as TJSONObject;
  try
    jsonString := TJson.Format(jsonObj);
    try
      TFile.WriteAllText(AFileName, jsonString);
    except
      on Error: Exception do
      begin
        Error.Log();
      end;
    end;
  finally
    jsonObj.free;
    FreeAndNil(serializer);
  end;

  result := 0;
end;

end.
