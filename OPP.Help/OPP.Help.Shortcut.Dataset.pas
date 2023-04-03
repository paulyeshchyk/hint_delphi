unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.Help.Map;

type
  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpMap>;
    function GetList: TList<TOPPHelpMap>;
  public
    constructor Create;
    function load(AFilename: String): Integer;
    function getMapping(key: String): TOPPHelpMap;
    function addMap(AMap: TOPPHelpMap): Integer;

    property list: TList<TOPPHelpMap> read GetList;
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
      Map: TOPPHelpMap;
    begin
      fShortcutHelpMatrix.Clear;
      if Assigned(Error) then
      begin
        Error.Log();
        exit;
      end;

      for Map in AList do
      begin
        self.addMap(Map);
      end;
    end;

  TOPPHelpMap.readJSON(AFilename, callback);
  result := 0;
end;

function TOPPHelpShortcutDataset.GetList: TList<TOPPHelpMap>;
var pair: TPair<String, TOPPHelpMap>;
begin
  result := TList<TOPPHelpMap>.Create;
  for pair in fShortcutHelpMatrix.ToArray do begin
    result.Add(pair.Value);
  end;
end;

function TOPPHelpShortcutDataset.addMap(AMap: TOPPHelpMap): Integer;
begin
  result := 0;
  if not Assigned(AMap) then
  begin
    result := -1;
    exit;
  end;
  self.fShortcutHelpMatrix.add(AMap.identifier, AMap);

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
        e.Log();
      end;
    end;

  finally
    result := Mapping;
  end;
end;

end.
