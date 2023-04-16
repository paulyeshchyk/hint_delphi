unit OPP.Help.System.Codable.TunningEditorDefaultSettings;

interface

uses
  OPP.Help.System.Codable;

type
  TOPPHelpHintTunningEditorDefaultSettings = class(TOPPCodable)
  private
    fHintsFilePath: String;
    fShortcutFilePath: String;
  public
    destructor Destroy; override;
    procedure SetDefaults(); override;
    property HintsFilePath: String read fHintsFilePath write fHintsFilePath;
    property ShortcutFilePath: String read fShortcutFilePath write fShortcutFilePath;
  end;

implementation

{ TOPPHelpSystemSettingEditorDefaults }

destructor TOPPHelpHintTunningEditorDefaultSettings.Destroy;
begin
  //
  inherited;
end;

procedure TOPPHelpHintTunningEditorDefaultSettings.SetDefaults;
const
  SDefaultHintData = '.\Документация\hint.data';
  SDefaultHelpData = '.\Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf';
begin
  inherited;
  HintsFilePath := SDefaultHintData;
  ShortcutFilePath := SDefaultHelpData;
end;

end.
