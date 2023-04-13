unit OPP.Help.Settings.Manager;

interface

uses
  System.SysUtils;

type
  TOPPHelpDefaults = class;
  TOPPHelpSettingsManagerReadCompletion = reference to procedure(AResult: TOPPHelpDefaults; error: Exception);
  TOPPHelpSettingsManagerSaveCompletion = reference to procedure(error: Exception);

  TOPPHelpDefaults = class
  private
    fHintsFilePath: String;
    fShortcutFilePath: String;
  public
    property HintsFilePath: String read fHintsFilePath write fHintsFilePath;
    property ShortcutFilePath: String read fShortcutFilePath write fShortcutFilePath;
  end;

  TOPPHelpSettingsManager = class
  public
    class procedure saveSettings(ASettings: TOPPHelpDefaults; completion: TOPPHelpSettingsManagerSaveCompletion);
    class procedure readSettings(completion: TOPPHelpSettingsManagerReadCompletion);
    class procedure defaultSettings(completion: TOPPHelpSettingsManagerReadCompletion);
  end;

implementation

uses
  OPP.Help.predicate, OPP.Help.System.Types,
  OPP.Help.log,
  OPP.Help.System.error,
  OPP.Help.System.Files,
  System.IOUtils, System.JSON, DBXJSONReflect, REST.JSON;

const
  kContext = 'SettingsManager';

resourcestring
  SSettingsFileName = 'defaults.json';
  SFileNotFoundMessage = 'File not found';
  SDefaultHelpIdx = '.\Документация\help.idx';
  SDefaultHintIdx = '.\Документация\hint.idx';

  { TOPPHelpSettingsManager }

class procedure TOPPHelpSettingsManager.defaultSettings(completion: TOPPHelpSettingsManagerReadCompletion);
var
  defaults: TOPPHelpDefaults;
begin
  if not assigned(completion) then
  begin
    exit;
  end;

  defaults := TOPPHelpDefaults.Create;
  defaults.HintsFilePath := SDefaultHintIdx;
  defaults.ShortcutFilePath := SDefaultHelpIdx;
  completion(defaults, nil);

end;

class procedure TOPPHelpSettingsManager.readSettings(completion: TOPPHelpSettingsManagerReadCompletion);
var
  fFileName: String;
  fBuffer: System.TArray<System.Byte>;
  fSerializer: TJSONUnMarshal;
  data: TJSONValue;
  fResult: TOPPHelpDefaults;
const
  isUTF8 = true;
begin
  fFileName := TOPPHelpSystemFilesHelper.AbsolutePath(SSettingsFileName);
  if not FileExists(fFileName) then
  begin
    if assigned(completion) then
      completion(nil, Exception.Create(SFileNotFoundMessage));
    exit;
  end;

  fSerializer := TJSONUnMarshal.Create;
  try
    try
      fBuffer := TFile.ReadAllBytes(fFileName);
      data := TJSONObject.ParseJSONValue(fBuffer, 0, isUTF8) as TJSONObject;
      fResult := fSerializer.Unmarshal(data) as TOPPHelpDefaults;
      if assigned(completion) then
        completion(fResult, nil);
    except
      on E: Exception do
      begin
        if assigned(completion) then
          completion(nil, E);
      end;
    end;
  finally
    fSerializer.Free;
    data.Free;
  end;
end;

class procedure TOPPHelpSettingsManager.saveSettings(ASettings: TOPPHelpDefaults; completion: TOPPHelpSettingsManagerSaveCompletion);
var
  fSerializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
  fFileName: String;
begin
  fSerializer := TJSONMarshal.Create;
  jsonObj := fSerializer.marshal(ASettings) as TJSONObject;
  try
    jsonString := TJson.JsonEncode(jsonObj);
    try
      fFileName := TOPPHelpSystemFilesHelper.AbsolutePath(SSettingsFileName);
      TFile.WriteAllText(fFileName, jsonString);
      if assigned(completion) then
        completion(nil);
    except
      on error: Exception do
      begin
        if assigned(completion) then
          completion(error);
      end;
    end;
  finally
    jsonObj.Free;
    FreeAndNil(fSerializer);
  end;

end;

end.
