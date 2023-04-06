unit OPP.Help.Shortcut.Dataset;

interface

uses
  System.Generics.Collections,
  OPP.Help.Map;

type
  TOPPHelpShortcutDatasetType = TDictionary<String, TOPPHelpMap>;

  TOPPHelpShortcutDataset = class
  private
    fShortcutHelpMatrix: TOPPHelpShortcutDatasetType;
    function GetList: TList<TOPPHelpMap>;
    procedure Merge(AList: TList<TOPPHelpMap>);
    procedure SetNewList(AList: TList<TOPPHelpMap>);
  public
    constructor Create;
    destructor Destroy; override;
    function load(AFilename: String): Integer;
    function GetMapping(const key: String): TOPPHelpMap;
    function addMap(const AMap: TOPPHelpMap): Integer;

    property list: TList<TOPPHelpMap> read GetList;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows,
  OPP.Help.Log,
  OPP.Help.System.Error,
  OPP.Help.Shortcut.Mapping.Filereader;

constructor TOPPHelpShortcutDataset.Create;
begin
  fShortcutHelpMatrix := TOPPHelpShortcutDatasetType.Create;
end;

destructor TOPPHelpShortcutDataset.Destroy;
begin
  fShortcutHelpMatrix.Free;
  inherited;
end;

function TOPPHelpShortcutDataset.load(AFilename: string): Integer;
begin
  TOPPHelpMap.readJSON(AFilename, SetNewList);
  result := 0;
end;

procedure TOPPHelpShortcutDataset.SetNewList(AList: TList<TOPPHelpMap>);
begin
  fShortcutHelpMatrix.Clear;
  self.Merge(AList);
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
    self.addMap(Map);
  end;
end;

function TOPPHelpShortcutDataset.GetList: TList<TOPPHelpMap>;
var
  pair: TPair<String, TOPPHelpMap>;
begin
  result := TList<TOPPHelpMap>.Create;
  for pair in fShortcutHelpMatrix.ToArray do
  begin
    result.Add(pair.Value);
  end;
end;

function TOPPHelpShortcutDataset.addMap(const AMap: TOPPHelpMap): Integer;
begin
  result := 0;
  if not assigned(AMap) then
  begin
    result := -1;
    exit;
  end;

  if fShortcutHelpMatrix.ContainsKey(AMap.identifier) then
  begin
    eventLogger.Error(Format('TOPPHelpShortcutDataset is trying to insert duplicated id: %s', [AMap.identifier]));
    exit;
  end;

  fShortcutHelpMatrix.Add(AMap.identifier, AMap);
end;

function TOPPHelpShortcutDataset.GetMapping(const key: string): TOPPHelpMap;
begin
  result := nil;
  try
    fShortcutHelpMatrix.TryGetValue(key, result);
  except
    on e: Exception do
    begin
      e.Log(key);
    end;
  end;
end;

end.
