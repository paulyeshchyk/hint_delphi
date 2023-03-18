unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.Help.Shortcut.Mapping;

type
  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpMap>;

  public
    constructor Create;
    function load(AFilename: String): Integer;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpMap>.create;
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
        //OutputDebugString(error.ClassName.toWideChar);
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

end.
