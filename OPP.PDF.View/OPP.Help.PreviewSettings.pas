unit OPP.Help.PreviewSettings;

interface

uses
  OPP.Help.System.Codable;

type

  TOPPHelpPreviewZoomMode = (zmFitHeight = 899, zmFitWidth = 898, zmTwoColumns = 897, zmCustom = 896);

  TOPPHelpPreviewSettings = class(TOPPCodable)
  private
    fZoomScale: Integer;
    fZoomMode: Integer;
    function GetZoomMode: TOPPHelpPreviewZoomMode;
    procedure SetZoomMode(const Value: TOPPHelpPreviewZoomMode);
  public
    constructor Create; override;
    property ZoomScale: Integer read fZoomScale write fZoomScale;
    property ZoomMode: TOPPHelpPreviewZoomMode read GetZoomMode write SetZoomMode;
  end;

  TOPPHelpPreviewSettingsHelper = class helper for TOPPHelpPreviewSettings
  private
    class procedure Load(AFileName: String; out AData: TOPPHelpPreviewSettings);
    class procedure Save(AFileName: String; AData: TOPPHelpPreviewSettings); overload;
  public
    class function LoadOrCreate: TOPPHelpPreviewSettings;
    class procedure Save(AData: TOPPHelpPreviewSettings); overload;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  OPP.Help.Log,
  OPP.Help.System.Files,
  OPP.Help.System.Codable.Helper;

const
  SOPPPreviewSettingsFileName = 'OPPHelpPreview.settings';

  { TOPPHelpPreviewSettings }

constructor TOPPHelpPreviewSettings.Create;
begin
  inherited Create;
  fZoomScale := 78;
  fZoomMode := Integer(zmFitHeight);
end;

function TOPPHelpPreviewSettings.GetZoomMode: TOPPHelpPreviewZoomMode;
begin
  result := TOPPHelpPreviewZoomMode(fZoomMode);
end;

procedure TOPPHelpPreviewSettings.SetZoomMode(const Value: TOPPHelpPreviewZoomMode);
begin
  fZoomMode := Integer(Value);
end;

{ TOPPHelpPreviewSettingsHelper }

class function TOPPHelpPreviewSettingsHelper.LoadOrCreate: TOPPHelpPreviewSettings;
var
  fFilePath: String;
begin
  fFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SOPPPreviewSettingsFileName);
  if not TFile.Exists(fFilePath) then
  begin
    result := TOPPHelpPreviewSettings.Create;
    exit;
  end;

  TOPPHelpPreviewSettings.Load(SOPPPreviewSettingsFileName, result);
end;

class procedure TOPPHelpPreviewSettingsHelper.Save(AData: TOPPHelpPreviewSettings);
begin
  TOPPHelpPreviewSettings.Save(SOPPPreviewSettingsFileName, AData);
end;

class procedure TOPPHelpPreviewSettingsHelper.Load(AFileName: String; out AData: TOPPHelpPreviewSettings);
begin
  try
    TOPPCodableHelper<TOPPHelpPreviewSettings>.Decode(AFileName, AData);
  except
    on E: Exception do
    begin
      AData := nil;
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPHelpPreviewSettingsHelper.Save(AFileName: String; AData: TOPPHelpPreviewSettings);
begin
  try
    TOPPCodableHelper<TOPPHelpPreviewSettings>.Encode(AFileName, AData);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

end.

