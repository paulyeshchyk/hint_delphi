unit OPP.Buffer.Manager.Settings.Data;

interface

uses
  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper;

type
  TOPPBufferManagerSettingsData = class(TOPPCodable)
  private
    fCurrentFileName: String;
    fShortcut: Word;
    fRecordsCountLimit: Integer;
    fIsExternalAllowed: Boolean;
    fCanSaveFormFrame: Boolean;
    fUseRecordsCountLimit: Boolean;

  public
    class procedure Save(AFileName: String; AData: TOPPBufferManagerSettingsData);
    class procedure Load(AFileName: String; out AData: TOPPBufferManagerSettingsData);

    constructor Create;override;
    property CurrentFileName: String read fCurrentFileName write fCurrentFileName;

    property Shortcut: Word read fShortcut write fShortcut;
    property RecordsCountLimit: Integer read fRecordsCountLimit write fRecordsCountLimit;
    property IsExternalAllowed: Boolean read fIsExternalAllowed write fIsExternalAllowed;
    property CanSaveFormFrame: Boolean read fCanSaveFormFrame write fCanSaveFormFrame;
    property UseRecordsCountLimit: Boolean read fUseRecordsCountLimit write fUseRecordsCountLimit;
  end;

implementation

uses
  System.SysUtils,
  OPP.Help.Log;

const
  SOPPBufferManagerSettingsFileName = 'OPPBufferManager.settings';

{ TOPPBufferManagerSettingsData }

constructor TOPPBufferManagerSettingsData.Create;
begin
  inherited Create;
  self.Shortcut := 0;
  self.RecordsCountLimit := 20;
  self.IsExternalAllowed := false;
  self.CanSaveFormFrame := true;
  self.UseRecordsCountLimit := true;
end;

class procedure TOPPBufferManagerSettingsData.Load(AFileName: String; out AData: TOPPBufferManagerSettingsData);
begin
  try
    TOPPCodableHelper<TOPPBufferManagerSettingsData>.Decode(SOPPBufferManagerSettingsFileName, AData);
  except
    on E: Exception do
    begin
      AData := nil;
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPBufferManagerSettingsData.Save(AFileName: String; AData: TOPPBufferManagerSettingsData);
begin
  try
    TOPPCodableHelper<TOPPBufferManagerSettingsData>.Encode(AFileName, AData);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

end.
