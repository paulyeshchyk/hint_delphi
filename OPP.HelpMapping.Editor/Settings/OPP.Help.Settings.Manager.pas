unit OPP.Help.Settings.Manager;

interface

uses
  System.SysUtils,
  System.IOUtils, System.JSON, DBXJSONReflect, REST.JSON,
  OPP.Help.Defaults;

type

  TOPPHelpSettingsManagerSaveCompletion = reference to procedure(error: Exception);
  TOPPHelpSettingsManagerReadCompletion = reference to procedure(const AResult: TOPPHelpDefaults; const AError: Exception);
  TOPPHelpSettingsManagerValidateCompletion = reference to procedure(const AResult: TOPPHelpDefaults; const AError: Exception; onSaveCompletion: TOPPHelpSettingsManagerReadCompletion);

  ICodable = interface
    procedure Encode(completion: TOPPHelpSettingsManagerSaveCompletion);
  end;

  TOPPHelpSettingsManager = class
  private
    class procedure Deserialize(data: TJSONObject; completion: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
  public
    class procedure DefaultSettings(AOnValidate: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
  end;

  TOPPHelpSettingsManagerHelper = class helper for TOPPHelpSettingsManager
  private
  end;

implementation

uses
  OPP.Help.predicate, OPP.Help.System.Types,
  OPP.Help.log,
  OPP.Help.System.error,
  OPP.Help.System.Files;

const
  kContext = 'SettingsManager';

resourcestring
  SErrorDecodeSettingsCompletionIsNotDefined = 'DecodeSettings completion is not defined';
  SEventSettingsWasSaved = 'settings was saved';
  SSettingsFileName = 'OPPHintTunning.settings';
  SFileNotFoundMessage = 'File not found';

const
  SDefaultHintData = '.\Документация\hint.data';
  SDefaultHelpData = '.\Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf';

  { TOPPHelpSettingsManager }

class procedure TOPPHelpSettingsManager.defaultSettings(AOnValidate: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
var
  fDefaults: TOPPHelpDefaults;
begin
  if not assigned(AOnValidate) then
  begin
    exit;
  end;

  fDefaults := TOPPHelpDefaults.Create;
  try
    fDefaults.HintsFilePath := SDefaultHintData;
    fDefaults.ShortcutFilePath := SDefaultHelpData;

    AOnValidate(fDefaults, nil, ACompletion);

  finally
    // fDefaults.Free;
  end;
end;

class procedure TOPPHelpSettingsManager.Deserialize(data: TJSONObject; completion: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
var
  fResult: TOPPHelpDefaults;
  fSerializer: TJSONUnMarshal;

begin
  if not assigned(completion) then
  begin
    exit;
  end;
  if not assigned(ACompletion) then
  begin
    exit;
  end;

  fSerializer := TJSONUnMarshal.Create;
  try
    try
      fResult := fSerializer.Unmarshal(data) as TOPPHelpDefaults;
      completion(fResult, nil, ACompletion);
    except
      on Error: Exception do
      begin
        completion(nil, Error, ACompletion);
      end;
    end;
  finally
    fSerializer.Free;
  end;

end;

{ TOPPHelpSettingsManagerHelper }

end.
