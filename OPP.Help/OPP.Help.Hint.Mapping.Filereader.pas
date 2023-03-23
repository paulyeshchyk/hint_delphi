unit OPP.Help.Hint.Mapping.Filereader;

interface
uses
  System.Generics.Collections,
  System.SysUtils,
  System.JSON, System.IOUtils,
  OPP.Help.System.Error,
  OPP.Help.Hint.Mapping;

type
  TOPPHelpHintMapJSONReadCallback = reference to procedure(AList: TList<TOPPHelpHintMap>; error: Exception);

  TOPPHelpHintMapFileReader = class helper for TOPPHelpHintMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpHintMapJSONReadCallback = nil);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpHintMapJSONReadCallback);
    class function saveJSON(AList: TList<TOPPHelpHintMap>; AFileName: String): Integer;
  end;

implementation

uses
  DBXJSONReflect, REST.JSON;

class procedure TOPPHelpHintMapFileReader.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpHintMapJSONReadCallback);
var
  deSerializer: TJSONUnMarshal;
  jsonObject: TJSONObject;
  mapList: TOPPHelpHintMapSet;
  list: TList<TOPPHelpHintMap>;
  error: Exception;
begin
  error := nil;
  deSerializer := TJSONUnMarshal.Create;
  list := TList<TOPPHelpHintMap>.Create;
  try
    jsonObject := TJSONObject.ParseJSONValue(ABytes, 0, isUTF8) as TJSONObject;
    try
      mapList := deSerializer.unmarshal(jsonObject) as TOPPHelpHintMapSet;
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

class procedure TOPPHelpHintMapFileReader.readJSON(AFileName: String; callback: TOPPHelpHintMapJSONReadCallback);
var
  bytes: System.TArray<System.Byte>;
begin
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

class function TOPPHelpHintMapFileReader.saveJSON(AList: TList<TOPPHelpHintMap>; AFileName: String): Integer;
var
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
begin
  //
  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(TOPPHelpHintMapSet.Create(AList)) as TJSONObject;
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
