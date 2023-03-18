unit OPP.Help.Shortcut.Mapping;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.IOUtils;

type

  TOPPHelpShortcutMap = class(TObject)
  private
    fHelpKeyword: String;
    fSearchPattern: String;
    fTestData: String;
  public
    constructor Create(AHelpKeyword: String; ASearchPattern: String);

    property TestData: String read fTestData write fTestData;
    property HelpKeyword: String read fHelpKeyword write fHelpKeyword;
    property SearchPattern: String read fSearchPattern write fSearchPattern;
  end;

  TOPPHelpShortcutMapSet = class(TObject)
  private
    fList: TList<TOPPHelpShortcutMap>;
  public
    property list: TList<TOPPHelpShortcutMap> read fList write fList;

    constructor Create(AList: TList<TOPPHelpShortcutMap>);
  end;

  TOPPHelpMapJSONReadCallback = reference to procedure(AList: TList<TOPPHelpShortcutMap>; error: Exception);

  TOPPHelpMapFileReader = class helper for TOPPHelpShortcutMap
  private
    class procedure parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean = true; callback: TOPPHelpMapJSONReadCallback = nil);
  public
    class procedure readJSON(AFileName: String; callback: TOPPHelpMapJSONReadCallback);
    class function saveJSON(AList: TList<TOPPHelpShortcutMap>; AFileName: String): Integer;
  end;

implementation

uses
  DBXJSON, DBXJSONReflect, REST.JSON;

constructor TOPPHelpShortcutMapSet.Create(AList: TList<TOPPHelpShortcutMap>);
begin
  fList := TList<TOPPHelpShortcutMap>.Create;
  fList.AddRange(AList);
end;

constructor TOPPHelpShortcutMap.Create(AHelpKeyword: String; ASearchPattern: String);
begin
  inherited Create;
  fHelpKeyword := AHelpKeyword;
  fSearchPattern := ASearchPattern;
end;

class function TOPPHelpMapFileReader.saveJSON(AList: TList<TOPPHelpShortcutMap>; AFileName: String): Integer;
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
      on E: Exception do begin
        //
      end;
    end;
  finally
    jsonObj.free;
    FreeAndNil(serializer);
  end;

  result := 0;
end;

class procedure TOPPHelpMapFileReader.readJSON(AFileName: String; callback: TOPPHelpMapJSONReadCallback);
var
  bytes: System.TArray<System.Byte>;
begin
  try
    try
      bytes := TFile.ReadAllBytes(AFileName);
    except
      on E: Exception do begin

      end;

    end;
  finally
    parseJSONBytes(bytes, true, callback);
  end;
end;

class procedure TOPPHelpMapFileReader.parseJSONBytes(ABytes: System.TArray<System.Byte>; isUTF8: Boolean; callback: TOPPHelpMapJSONReadCallback);
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
      on E: Exception do begin
        error := E;
      end;
    end;
  finally
    if assigned(callback) then begin
      callback(list, error);
    end;
    FreeAndNil(jsonObject);
    FreeAndNil(list);
    FreeAndNil(deSerializer);
  end;

end;

end.
