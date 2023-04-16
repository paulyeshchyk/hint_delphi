unit OPP.Help.Defaults;

interface

type

  IOPPHelpDefaults = interface
    function GetHintsFilePath(): String;
    function GetShortcutFilePath(): String;
    procedure SetHintsFilePath(AValue: String);
    procedure SetShortcutFilePath(AValue: String);
    property HintsFilePath: String read GetHintsFilePath write SetHintsFilePath;
    property ShortcutFilePath: String read GetShortcutFilePath write SetShortcutFilePath;
  end;

  TOPPHelpDefaults = class(TInterfacedObject, IOPPHelpDefaults)
  private
    fHintsFilePath: String;
    fShortcutFilePath: String;
  public
    function GetHintsFilePath(): String;
    function GetShortcutFilePath(): String;
    procedure SetHintsFilePath(AValue: String);
    procedure SetShortcutFilePath(AValue: String);
    property HintsFilePath: String read fHintsFilePath write fHintsFilePath;
    property ShortcutFilePath: String read fShortcutFilePath write fShortcutFilePath;
  end;

implementation

{ TOPPHelpDefaults }

function TOPPHelpDefaults.GetHintsFilePath: String;
begin
  result := fHintsFilePath
end;

function TOPPHelpDefaults.GetShortcutFilePath: String;
begin
  result := fShortcutFilePath
end;

procedure TOPPHelpDefaults.SetHintsFilePath(AValue: String);
begin
  fHintsFilePath := AValue;
end;

procedure TOPPHelpDefaults.SetShortcutFilePath(AValue: String);
begin
  fShortcutFilePath := AValue;
end;

end.
