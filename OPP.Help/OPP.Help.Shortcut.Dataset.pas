unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,

  OPP.Help.Shortcut.Mapping;

type
  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpShortcutMap>;
  public
    constructor Create;
    function load(AFilename: String): Integer;
    function getMapping(key: String): TOPPHelpShortcutMap;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows,
  OPP.Help.System.Error,
  OPP.Help.Shortcut.Mapping.Filereader;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpShortcutMap>.Create;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
var
  callback: TOPPHelpShortcutMapJSONReadCallback;
begin
  callback := procedure(AList: TList<TOPPHelpShortcutMap>; Error: Exception)
    var
      map: TOPPHelpShortcutMap;
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

  TOPPHelpShortcutMap.readJSON(AFilename, callback);
  result := 0;
end;

function TOPPHelpShortcutDataset.GetMapping(key: string): TOPPHelpShortcutMap;
var
  Mapping: TOPPHelpShortcutMap;
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
