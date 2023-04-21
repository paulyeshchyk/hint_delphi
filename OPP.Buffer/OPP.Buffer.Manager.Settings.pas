unit OPP.Buffer.Manager.Settings;

interface

uses
  OPP.Buffer.Manager.Settings.Data;

type
  IOPPBufferManagerSettings = interface
    function GetDefaultFilePath: String;

    procedure SetCurrentFilePath(AFilePath: String);
    function GetCurrentFilePath: String;
    function isExternalAllowed: Boolean;
  end;

  TOPPBufferManagerSettings = class(TInterfacedObject, IOPPBufferManagerSettings)
  private
    fData: TOPPBufferManagerSettingsData;
    function LoadDataOrCreate: TOPPBufferManagerSettingsData;
  public
    constructor Create;
    destructor Destroy; override;

    function isExternalAllowed: Boolean;
    function GetDefaultFilePath: String;

    procedure SetCurrentFilePath(AFilePath: String);
    function GetCurrentFilePath: String;
  end;

implementation

uses
  OPP.Help.System.Files,
  System.IOUtils;

const
  SClipboardFileName = 'OPPBufferManager.oppclipboarddata';
  SOPPBufferManagerSettingsFileName = 'OPPBufferManager.settings';

{ TOPPBufferManagerSettings }

constructor TOPPBufferManagerSettings.Create;
begin
  inherited;

  fData := LoadDataOrCreate();
end;

destructor TOPPBufferManagerSettings.Destroy;
begin
  fData.Free;
  inherited;
end;

function TOPPBufferManagerSettings.GetCurrentFilePath: String;
begin
  result := fData.CurrentFileName;
end;

function TOPPBufferManagerSettings.GetDefaultFilePath: String;
begin
  result := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SClipboardFileName);
end;

function TOPPBufferManagerSettings.isExternalAllowed: Boolean;
begin
  result := true;
end;

function TOPPBufferManagerSettings.LoadDataOrCreate: TOPPBufferManagerSettingsData;
var
  fResult: TOPPBufferManagerSettingsData;
  fFilePath: String;
begin
  fFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SOPPBufferManagerSettingsFileName);
  if not TFile.Exists(fFilePath) then
  begin
    fResult := TOPPBufferManagerSettingsData.Create;
    result := fResult;
    exit;
  end;

  TOPPBufferManagerSettingsData.Load(SOPPBufferManagerSettingsFileName, result);
end;

procedure TOPPBufferManagerSettings.SetCurrentFilePath(AFilePath: String);
begin
  fData.CurrentFileName := AFilePath;
  TOPPBufferManagerSettingsData.Save(SOPPBufferManagerSettingsFileName, fData);
end;

end.
