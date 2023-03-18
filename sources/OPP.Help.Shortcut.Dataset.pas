unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.System,
  OPP.Help.Shortcut.Mapping;

type
  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpMap>;
  public
    constructor Create;
    function load(AFilename: String): Integer;
    function getMapping(key: String): TOPPHelpMap;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpMap>.Create;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
var
  callback: TOPPHelpMapJSONReadCallback;
begin
  callback := procedure(AList: TList<TOPPHelpMap>; error: Exception)
    var
      map: TOPPHelpMap;
    begin
      fShortcutHelpMatrix.Clear;
      if Assigned(error) then
      begin
        // OutputDebugString(error.ClassName.toWideChar);
        exit;
      end;
      for map in AList do
      begin
        if Assigned(map) then
        begin
          self.fShortcutHelpMatrix.add(map.HelpKeyword, map);
        end;
      end;
    end;

  TOPPHelpMap.readJSON(AFilename, callback);
  result := 0;
end;

function TOPPHelpShortcutDataset.getMapping(key: string): TOPPHelpMap;
var
  Mapping: TOPPHelpMap;
begin
  try
    Mapping := nil;
    try
      fShortcutHelpMatrix.TryGetValue(key, Mapping);
    except
      on e: Exception do
      begin
        //
      end;
    end;

  finally
    result := Mapping;
  end;
end;


end.
