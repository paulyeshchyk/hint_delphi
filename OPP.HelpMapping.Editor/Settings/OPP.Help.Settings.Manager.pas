unit OPP.Help.Settings.Manager;

interface

uses
  System.SysUtils;

type
  TOPPHelpDefaults = class;
  TOPPHelpSettingsManagerSaveCompletion = reference to procedure(error: Exception);
  TOPPHelpSettingsManagerReadCompletion = reference to procedure(const AResult: TOPPHelpDefaults; const AError: Exception);
  TOPPHelpSettingsManagerValidateCompletion = reference to procedure(const AResult: TOPPHelpDefaults; const AError: Exception; onSaveCompletion: TOPPHelpSettingsManagerReadCompletion);

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
    class procedure encodeSettings(ASettings: TOPPHelpDefaults; completion: TOPPHelpSettingsManagerSaveCompletion);
    class procedure decodeSettings(completion: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
    class procedure defaultSettings(AOnValidate: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
  end;

  TOPPHelpSettingsManagerHelper = class helper for TOPPHelpSettingsManager
  private
    class procedure OnSettingsSave(const AResult: TOPPHelpDefaults; const AError: Exception; ACompletion: TOPPHelpSettingsManagerReadCompletion);
    class procedure OnSettingsRead(const AResult: TOPPHelpDefaults; const AError: Exception; ACompletion: TOPPHelpSettingsManagerReadCompletion);
  public
    class procedure createDefaultSettingsIfNeed(postCompletion: TOPPHelpSettingsManagerReadCompletion);
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

class procedure TOPPHelpSettingsManager.decodeSettings(completion: TOPPHelpSettingsManagerValidateCompletion; ACompletion: TOPPHelpSettingsManagerReadCompletion);
var
  fFileName: String;
  fBuffer: System.TArray<System.Byte>;
  fSerializer: TJSONUnMarshal;
  data: TJSONValue;
  fResult: TOPPHelpDefaults;
  error: Exception;
const
  isUTF8 = true;
begin
  fFileName := TOPPHelpSystemFilesHelper.AbsolutePath(SSettingsFileName);
  if not FileExists(fFileName) then
  begin
    if assigned(completion) then
    begin
      error := Exception.Create(SFileNotFoundMessage);
      try
        completion(nil, error, ACompletion);
      finally
        error.Free;
      end;
    end;
    exit;
  end;

  fSerializer := TJSONUnMarshal.Create;
  try
    try
      fBuffer := TFile.ReadAllBytes(fFileName);
      data := TJSONObject.ParseJSONValue(fBuffer, 0, isUTF8) as TJSONObject;
      fResult := fSerializer.Unmarshal(data) as TOPPHelpDefaults;
      if assigned(completion) then
        completion(fResult, nil, ACompletion);
    except
      on E: Exception do
      begin
        if assigned(completion) then
          completion(nil, E, ACompletion);
      end;
    end;
  finally
    fSerializer.Free;
    data.Free;
  end;
end;

class procedure TOPPHelpSettingsManager.encodeSettings(ASettings: TOPPHelpDefaults; completion: TOPPHelpSettingsManagerSaveCompletion);
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

{ TOPPHelpSettingsManagerHelper }

class procedure TOPPHelpSettingsManagerHelper.createDefaultSettingsIfNeed(postCompletion: TOPPHelpSettingsManagerReadCompletion);
begin
  TOPPHelpSettingsManager.decodeSettings(OnSettingsRead, postCompletion);
end;

class procedure TOPPHelpSettingsManagerHelper.OnSettingsRead(const AResult: TOPPHelpDefaults; const AError: Exception; ACompletion: TOPPHelpSettingsManagerReadCompletion);
begin
  if assigned(AError) then
  begin
    eventLogger.error(AError, kContext);
    TOPPHelpSettingsManager.defaultSettings(OnSettingsSave, ACompletion);
    exit;
  end;

  if assigned(ACompletion) then
  begin
    ACompletion(AResult, nil);
  end;

end;

class procedure TOPPHelpSettingsManagerHelper.OnSettingsSave(const AResult: TOPPHelpDefaults; const AError: Exception; ACompletion: TOPPHelpSettingsManagerReadCompletion);
begin
  if assigned(AError) then
  begin
    eventLogger.error(AError, kContext);
    if assigned(ACompletion) then
    begin
      ACompletion(nil, AError);
    end;

    exit;
  end;

  // save default
  TOPPHelpSettingsManager.encodeSettings(AResult,
    procedure(ASaveError: Exception)
    begin
      if assigned(ASaveError) then
        eventLogger.error(ASaveError, kContext)
      else
        eventLogger.Flow('settings was saved', kContext);

      if assigned(ACompletion) then
      begin
        ACompletion(AResult, ASaveError);
      end;
    end);

end;

end.
