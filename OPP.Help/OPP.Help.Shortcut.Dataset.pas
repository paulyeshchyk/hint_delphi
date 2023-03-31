unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.Help.Map;

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
  System.SysUtils, Winapi.Windows,
  OPP.Help.System.Error,
  OPP.Help.Shortcut.Mapping.Filereader;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpMap>.Create;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
var
  callback: TOPPHelpShortcutMapJSONReadCallback;
begin
  callback := procedure(AList: TList<TOPPHelpMap>; Error: Exception)
    var
      map: TOPPHelpMap;
    begin
      fShortcutHelpMatrix.Clear;
      if Assigned(Error) then
      begin
        Error.Log();
        exit;
      end;

      for map in AList do
      begin
        if Assigned(map) then
          self.fShortcutHelpMatrix.add(map.identifier, map);
      end;
    end;

  TOPPHelpMap.readJSON(AFilename, callback);
  result := 0;
end;

function TOPPHelpShortcutDataset.GetMapping(key: string): TOPPHelpMap;
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
        e.Log();
      end;
    end;

  finally
    result := Mapping;
  end;
end;

end.
