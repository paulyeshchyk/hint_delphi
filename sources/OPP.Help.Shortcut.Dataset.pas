unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.System,
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
  System.SysUtils,
  Winapi.Windows,
  OPP.Help.Shortcut.Mapping.Filereader;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpShortcutMap>.Create;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
var
  callback: TOPPHelpShortcutMapJSONReadCallback;
begin
  callback := procedure(AList: TList<TOPPHelpShortcutMap>; error: Exception)
    var
      map: TOPPHelpShortcutMap;
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

  TOPPHelpShortcutMap.readJSON(AFilename, callback);
  result := 0;
end;

function TOPPHelpShortcutDataset.getMapping(key: string): TOPPHelpShortcutMap;
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
        //
      end;
    end;

  finally
    result := Mapping;
  end;
end;


end.
