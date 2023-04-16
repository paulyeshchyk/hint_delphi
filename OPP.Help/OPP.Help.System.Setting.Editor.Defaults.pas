unit OPP.Help.System.Setting.Editor.Defaults;

interface

uses
  System.SysUtils;

type
  TOPPJsonDecodeException = class(Exception);
  TOPPJsonEncodeException = class(Exception);

  TOPPHelpHintTunningEditorDefaultSettings = class
  private
    fHintsFilePath: String;
    fShortcutFilePath: String;
  public
    constructor Create;
    destructor Destroy; override;
    property HintsFilePath: String read fHintsFilePath write fHintsFilePath;
    property ShortcutFilePath: String read fShortcutFilePath write fShortcutFilePath;
  end;

  TOPPHelpSystemSettingEditorDefaultsHelper = class helper for TOPPHelpHintTunningEditorDefaultSettings
  private
    class function CreateDefaults(AFileName: String): TOPPHelpHintTunningEditorDefaultSettings;
    class function ReCreateDefaults(AFileName: String): TOPPHelpHintTunningEditorDefaultSettings;
    class procedure SetDefaults(AValue: TOPPHelpHintTunningEditorDefaultSettings);
    class procedure SetDefaultsAndSave(AValue: TOPPHelpHintTunningEditorDefaultSettings; AFileName: String);
  public
    class procedure Decode(AFileName: String; out AValue: TOPPHelpHintTunningEditorDefaultSettings);
    class procedure Encode(AFileName: String; AValue: TOPPHelpHintTunningEditorDefaultSettings);
  end;

implementation

uses OPP.Help.System.Codable, OPP.Help.System.Files, OPP.Help.Log;

const
  SDefaultHintData = '.\Документация\hint.data';
  SDefaultHelpData = '.\Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf';

class function TOPPHelpSystemSettingEditorDefaultsHelper.CreateDefaults(AFileName: String): TOPPHelpHintTunningEditorDefaultSettings;
begin
  result := TOPPHelpHintTunningEditorDefaultSettings.Create;
  try
    SetDefaultsAndSave(result, AFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPHelpSystemSettingEditorDefaultsHelper.Decode(AFileName: String; out AValue: TOPPHelpHintTunningEditorDefaultSettings);
var
  fOPPDecoder: TOPPCoder<TOPPHelpHintTunningEditorDefaultSettings>;
  fFileName: String;
begin

  AValue := nil;

  fFileName := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFileName);

  fOPPDecoder := TOPPCoder<TOPPHelpHintTunningEditorDefaultSettings>.Create;
  try
    try
      fOPPDecoder.DecodeFromJSON(fFileName, true, AValue);
    except
      on E: OPPFileNotFoundException do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        AValue := CreateDefaults(AFileName);
      end;
      on E: OPPCoderDeserializeException do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        AValue := ReCreateDefaults(AFileName);
      end;
      on E: Exception do
      begin
        eventLogger.Error(E);
        if assigned(AValue) then
          FreeAndNil(AValue);
        raise TOPPJsonDecodeException.Create('');
      end;
    end;
  finally
    fOPPDecoder.Free;
  end;
end;

class procedure TOPPHelpSystemSettingEditorDefaultsHelper.Encode(AFileName: String; AValue: TOPPHelpHintTunningEditorDefaultSettings);
var
  fOPPDecoder: TOPPCoder<TOPPHelpHintTunningEditorDefaultSettings>;
  fFileName: String;
begin

  fFileName := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFileName);

  fOPPDecoder := TOPPCoder<TOPPHelpHintTunningEditorDefaultSettings>.Create;
  try
    try
      fOPPDecoder.EncodeToJSON(fFileName, AValue);
    except
      on E: Exception do
      begin
        eventLogger.Error(E);
        raise TOPPJsonEncodeException.Create('');
      end;
    end;
  finally
    fOPPDecoder.Free;
  end;

end;

class function TOPPHelpSystemSettingEditorDefaultsHelper.ReCreateDefaults(AFileName: String): TOPPHelpHintTunningEditorDefaultSettings;
begin
  result := TOPPHelpHintTunningEditorDefaultSettings.Create;
  try
    SetDefaultsAndSave(result, AFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
      if assigned(result) then
        FreeAndNil(result);
      raise OPPCoderRecreateException.Create('');
    end;
  end;
end;

class procedure TOPPHelpSystemSettingEditorDefaultsHelper.SetDefaults(AValue: TOPPHelpHintTunningEditorDefaultSettings);
begin
  if not assigned(AValue) then
    exit;
  AValue.HintsFilePath := SDefaultHintData;
  AValue.ShortcutFilePath := SDefaultHelpData;
end;

class procedure TOPPHelpSystemSettingEditorDefaultsHelper.SetDefaultsAndSave(AValue: TOPPHelpHintTunningEditorDefaultSettings; AFileName: String);
begin
  if not assigned(AValue) then
    exit;

  SetDefaults(AValue);

  Self.Encode(AFileName, AValue);

end;

{ TOPPHelpSystemSettingEditorDefaults }

constructor TOPPHelpHintTunningEditorDefaultSettings.Create;
begin
  inherited Create;
end;

destructor TOPPHelpHintTunningEditorDefaultSettings.Destroy;
begin
  //
  inherited;
end;

end.
