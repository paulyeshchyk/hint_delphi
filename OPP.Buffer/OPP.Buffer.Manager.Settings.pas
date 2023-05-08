unit OPP.Buffer.Manager.Settings;

interface

uses
  System.Types,
  OPP.Buffer.Manager.Settings.Data,
  OPP.Help.System.Codable.FormSizeSettings;

type

  IOPPBufferManagerSettings = interface
    function GetDefaultFilePath: String;

    procedure SetColumnSort(AValue: TArray<TOPPBufferManagerSettingsColumnSort>);
    function GetColumnSort: TArray<TOPPBufferManagerSettingsColumnSort>;

    procedure SetShortCut(AValue: Word);
    function GetShortCut: Word;

    procedure SetCanSaveFormFrame(AValue: Boolean);
    function GetCanSaveFormFrame: Boolean;

    procedure SetCurrentFilePath(AFilePath: String);
    function GetCurrentFilePath: String;

    procedure SetRecordsCountLimit(AValue: Integer);
    function GetRecordsCountLimit: Integer;

    procedure SetUseRecordsCountLimit(AValue: Boolean);
    function GetUseRecordsCountLimit: Boolean;

    procedure SetIsExternalAllowed(AValue: Boolean);
    function GetIsExternalAllowed: Boolean;

    procedure SetFormFrame(AFrame: TRect);
    function GetFormFrame: TRect;

    procedure SetAutoFilter(AValue: Boolean);
    function GetAutoFilter: Boolean;

    procedure Save;
  end;

  TOPPBufferManagerSettings = class(TInterfacedObject, IOPPBufferManagerSettings)
  private
    fData: TOPPBufferManagerSettingsData;

    function LoadDataOrCreate: TOPPBufferManagerSettingsData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetColumnSort(AValue: TArray<TOPPBufferManagerSettingsColumnSort>);
    function GetColumnSort: TArray<TOPPBufferManagerSettingsColumnSort>;

    procedure SetShortCut(AValue: Word);
    function GetShortCut: Word;

    procedure SetCanSaveFormFrame(AValue: Boolean);
    function GetCanSaveFormFrame: Boolean;

    procedure SetRecordsCountLimit(AValue: Integer);
    function GetRecordsCountLimit: Integer;

    procedure SetIsExternalAllowed(AValue: Boolean);
    function GetIsExternalAllowed: Boolean;

    procedure SetUseRecordsCountLimit(AValue: Boolean);
    function GetUseRecordsCountLimit: Boolean;

    function GetDefaultFilePath: String;

    procedure SetCurrentFilePath(AFilePath: String);
    function GetCurrentFilePath: String;

    procedure SetFormFrame(AFrame: TRect);
    function GetFormFrame: TRect;

    procedure SetAutoFilter(AValue: Boolean);
    function GetAutoFilter: Boolean;

    procedure Save;
  end;

implementation

uses
  OPP.Help.System.Files,
  OPP.Help.Log,
  System.SysUtils,
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

function TOPPBufferManagerSettings.GetAutoFilter: Boolean;
begin
  result := fData.AutoFilter;
end;

function TOPPBufferManagerSettings.GetCanSaveFormFrame: Boolean;
begin
  result := fData.CanSaveFormFrame;
end;

function TOPPBufferManagerSettings.GetColumnSort: TArray<TOPPBufferManagerSettingsColumnSort>;
begin
  result := fData.ColumnSort.ToArray;
end;

function TOPPBufferManagerSettings.GetCurrentFilePath: String;
begin
  result := fData.CurrentFileName;
end;

function TOPPBufferManagerSettings.GetDefaultFilePath: String;
begin
  result := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SClipboardFileName);
end;

function TOPPBufferManagerSettings.GetFormFrame: TRect;
begin
  result := fData.FormFrame;
end;

function TOPPBufferManagerSettings.GetIsExternalAllowed: Boolean;
begin
  result := fData.IsExternalAllowed;
end;

function TOPPBufferManagerSettings.GetRecordsCountLimit: Integer;
begin
  result := fData.RecordsCountLimit;
end;

function TOPPBufferManagerSettings.GetShortCut: Word;
begin
  result := fData.Shortcut;
end;

function TOPPBufferManagerSettings.GetUseRecordsCountLimit: Boolean;
begin
  result := fData.UseRecordsCountLimit;
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

procedure TOPPBufferManagerSettings.Save;
var
  fFilePath: String;
begin
  fFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SOPPBufferManagerSettingsFileName);
  if TFile.Exists(fFilePath) then begin
    try
      TFile.Delete(fFilePath);
    except
      on E: Exception do begin
        eventLogger.Error(E, 'TOPPBufferManagerSettings');
        raise E;
      end;
    end;
  end;

  TOPPBufferManagerSettingsData.Save(SOPPBufferManagerSettingsFileName, fData);
end;

procedure TOPPBufferManagerSettings.SetAutoFilter(AValue: Boolean);
begin
  fData.AutoFilter := AValue;
end;

procedure TOPPBufferManagerSettings.SetCanSaveFormFrame(AValue: Boolean);
begin
  fData.CanSaveFormFrame := AValue;
end;

procedure TOPPBufferManagerSettings.SetColumnSort(AValue: TArray<TOPPBufferManagerSettingsColumnSort>);
begin
  fData.SetColumnSortArray(AValue);
  //Run save
  Save;
end;

procedure TOPPBufferManagerSettings.SetCurrentFilePath(AFilePath: String);
begin
  fData.CurrentFileName := AFilePath;
  TOPPBufferManagerSettingsData.Save(SOPPBufferManagerSettingsFileName, fData);
end;

procedure TOPPBufferManagerSettings.SetFormFrame(AFrame: TRect);
begin
  if fData.CanSaveFormFrame then begin
    fData.FormFrame := AFrame;
    Save;
  end;
end;

procedure TOPPBufferManagerSettings.SetIsExternalAllowed(AValue: Boolean);
begin
  fData.IsExternalAllowed := AValue;
end;

procedure TOPPBufferManagerSettings.SetRecordsCountLimit(AValue: Integer);
begin
  fData.RecordsCountLimit := AValue;
end;

procedure TOPPBufferManagerSettings.SetShortCut(AValue: Word);
begin
  fData.Shortcut := AValue;
end;

procedure TOPPBufferManagerSettings.SetUseRecordsCountLimit(AValue: Boolean);
begin
  fData.UseRecordsCountLimit := AValue;
end;

end.
