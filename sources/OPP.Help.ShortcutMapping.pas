unit OPP.Help.ShortcutMapping;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.IOUtils;

type
  TOPPHelpMap = class
  private
    fHelpKeyword: String;
    fSearchPattern: String;
  public
    constructor Create(AHelpKeyword: String; ASearchPattern: String);

    property HelpKeyword: String read fHelpKeyword write fHelpKeyword;
    property SearchPattern: String read fSearchPattern write fSearchPattern;
  end;

  TOPPHelpMapFileReader = class helper for TOPPHelpMap
  public
    class function readJSON(AFileName: String): TList<TOPPHelpMap>;
  end;

implementation

constructor TOPPHelpMap.Create(AHelpKeyword: String; ASearchPattern: String);
begin
  inherited Create;
  fHelpKeyword := AHelpKeyword;
  fSearchPattern := ASearchPattern;
end;

class function TOPPHelpMapFileReader.readJSON(AFileName: String): TList<TOPPHelpMap>;
var
  s: String;
  bytes: TArray<System.Byte>;
  ja: TJSONArray;
  i: Integer;
  jo: TJSONObject;
  hc, sp: String;
begin
  result := TList<TOPPHelpMap>.Create;

  try
    s := TFile.ReadAllText(AFileName);
  finally
    bytes := TEncoding.UTF8.GetBytes(s);
    ja := TJSONObject.ParseJSONValue(bytes, 0) as TJSONArray;
    if assigned(ja) then begin
      if ja.Count <> 0 then begin
        for i := 0 to ja.Count - 1 do begin
          jo := ja.Items[i] as TJSONObject;
          hc := jo.Get('HelpKeyword').JsonValue.Value;
          sp := jo.Get('SearchPattern').JsonValue.Value;
          result.add(TOPPHelpMap.Create(hc, sp))
        end;
      end;
      ja.Free;
    end;
  end;
end;

end.
