unit OPP.Help.Shortcut.Mapping.Filereader;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON, System.IOUtils,
  OPP.Help.Shortcut.Mapping;

type
  TOPPHelpShortcutMapJSONReadCallback = reference to procedure(AList: TList<TOPPHelpShortcutMap>; error: Exception);

  TOPPHelpShortcutMapFileReader = class helper for TOPPHelpShortcutMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpShortcutMapJSONReadCallback = nil);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpShortcutMapJSONReadCallback);
    class function saveJSON(AList: TList<TOPPHelpShortcutMap>; AFileName: String): Integer;
  end;

implementation

uses
  DBXJSONReflect, REST.JSON;

class procedure TOPPHelpShortcutMapFileReader.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpShortcutMapJSONReadCallback);
var
  deSerializer: TJSONUnMarshal;
  jsonObject: TJSONObject;
  mapList: TOPPHelpShortcutMapSet;
  list: TList<TOPPHelpShortcutMap>;
  error: Exception;
begin
  error := nil;
  deSerializer := TJSONUnMarshal.Create;
  list := TList<TOPPHelpShortcutMap>.Create;
  try
    jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
    try
      mapList := deSerializer.unmarshal(jsonObject) as TOPPHelpShortcutMapSet;
      list.AddRange(mapList.list);
      FreeAndNil(mapList);
    except
      on E: Exception do
      begin
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

class procedure TOPPHelpShortcutMapFileReader.readJSON(AFileName: String; callback: TOPPHelpShortcutMapJSONReadCallback);
var
  bytes: System.TArray<System.Byte>;
begin
  try
    try
      bytes := TFile.ReadAllBytes(AFileName);
    except
      on E: Exception do
      begin

      end;

    end;
  finally
    parseJSONBytes(bytes, true, callback);
  end;
end;

class function TOPPHelpShortcutMapFileReader.saveJSON(AList: TList<TOPPHelpShortcutMap>; AFileName: String): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  //
  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(TOPPHelpShortcutMapSet.Create(AList)) as TJSONObject;
  try
    jsonString := TJson.Format(jsonObj); // formatted
    // jsonString := jsonObj.ToString;//unformatted
    try
      TFile.WriteAllText(AFileName, jsonString);
    except
      on E: Exception do
      begin
        //
      end;
    end;
  finally
    jsonObj.free;
    FreeAndNil(serializer);
  end;

  result := 0;
end;

end.
