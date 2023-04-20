unit OPP.Buffer.Manager.Settings.Data;

interface

uses
  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper;

type
  TOPPBufferManagerSettingsData = class(TOPPCodable)
  private
    fCurrentFileName: String;
  public
    class procedure Save(AFileName: String; AData: TOPPBufferManagerSettingsData);
    class procedure Load(AFileName: String; out AData: TOPPBufferManagerSettingsData);

    property CurrentFileName: String read fCurrentFileName write fCurrentFileName;
  end;

implementation

uses
  System.SysUtils,
  OPP.Help.Log;

const
  SOPPBufferManagerSettingsFileName = 'OPPBufferManager.settings';

{ TOPPBufferManagerSettingsData }

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
