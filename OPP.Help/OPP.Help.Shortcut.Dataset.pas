unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  OPP.Help.Map;

type
  TOPPHelpShortcutDatasetType = TDictionary<String, TOPPHelpMap>;

  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TOPPHelpShortcutDatasetType;
    procedure Merge(AList: TList<TOPPHelpMap>);
    procedure SetNewList(Mapset: TOPPHelpMapSet; error: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    function load(AFilename: String): Integer;
    function GetMapping(const key: String): TOPPHelpMap;
    function AddMap(const AMap: TOPPHelpMap): Integer;
    procedure RemoveMap(const AIdentifier: String);
    function List(): TList<TOPPHelpMap>;
  end;

implementation

uses
  Winapi.Windows,
  OPP.Help.Log,
  OPP.Help.System.error,
  OPP.Help.Map.Parser.JSON;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TOPPHelpShortcutDatasetType.Create;
end;

destructor TOPPHelpShortcutDataset.Destroy;
begin
  fShortcutHelpMatrix.Clear;
  fShortcutHelpMatrix.Free;
  inherited;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
begin
  TOPPHelpMapRESTParser.readJSON(AFilename, SetNewList);
  result := 0;
end;

procedure TOPPHelpShortcutDataset.SetNewList(Mapset: TOPPHelpMapSet; error: Exception);
var fMap: TOPPHelpMap;
begin
  if assigned(error) then
  begin
    eventLogger.error(error);
    exit;
  end;

  fShortcutHelpMatrix.Clear;
  for fMap in Mapset.list do begin
    self.AddMap(fMap);
  end;
end;

procedure TOPPHelpShortcutDataset.Merge(AList: TList<TOPPHelpMap>);
var
  Map: TOPPHelpMap;
begin
  if (not assigned(AList)) or (AList.Count = 0) then
  begin
    exit;
  end;

  for Map in AList do
  begin
    if not assigned(Map) then
      continue;
    self.AddMap(Map);
  end;
end;

procedure TOPPHelpShortcutDataset.RemoveMap(const AIdentifier: String);
begin
  fShortcutHelpMatrix.Remove(AIdentifier);
end;

function TOPPHelpShortcutDataset.AddMap(const AMap: TOPPHelpMap): Integer;
begin
  result := 0;
  if not assigned(AMap) then
  begin
    result := -1;
    exit;
  end;

  if fShortcutHelpMatrix.ContainsKey(AMap.ComponentIdentifier) then
  begin
    eventLogger.error(Format('TOPPHelpShortcutDataset is trying to insert duplicated id: %s', [AMap.ComponentIdentifier]));
    exit;
  end;

  fShortcutHelpMatrix.Add(AMap.ComponentIdentifier, AMap);
end;

function TOPPHelpShortcutDataset.GetMapping(const key: string): TOPPHelpMap;
begin
  result := nil;
  try
    fShortcutHelpMatrix.TryGetValue(key, result);
  except
    on e: Exception do
    begin
      eventLogger.error(Format('Error: %s; %s', [e.Message, key]));
    end;
  end;
end;

function TOPPHelpShortcutDataset.List: TList<TOPPHelpMap>;
var
  pair: TPair<String, TOPPHelpMap>;
begin
  result := TList<TOPPHelpMap>.Create();
  for pair in fShortcutHelpMatrix.ToArray do
  begin
    result.Add(pair.Value)
  end;
end;

end.
