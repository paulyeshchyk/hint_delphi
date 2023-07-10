unit OPP.Guide.Settings;

interface

uses
  System.Classes,
  System.Types, System.SysUtils,
  OPP.Help.Log,
  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper,
  OPP.Help.System.Files;

type
  TOPPGuideSettingsData = class;
  TOPPGuideOnFormFrameLoad = reference to procedure(AFrame: TRect);
  TOPPGuideOnFormFrameSave = reference to function: TRect;

  TOPPGuideOnLoadStringList = reference to procedure(AList: TStringList);
  TOPPGuideOnLoadFileName = reference to procedure(AFileName: String);
  TOPPGuideOnFilenameSet = reference to function: String;
  TOPPGuideOnFilenameWrite = reference to procedure(AFileName: String);

  IOPPGuideSettings = interface
    procedure Load;

    procedure OnFormFrameLoad(const Value: TOPPGuideOnFormFrameLoad);
    procedure OnFormFrameSave(const Value: TOPPGuideOnFormFrameSave);

    procedure OnDockingFileLoad(const Value: TOPPGuideOnLoadFileName);
    procedure OnDockingFilenameGet(const Value: TOPPGuideOnFilenameSet);
    procedure OnDockingFileSave(const Value: TOPPGuideOnFilenameWrite);

    procedure OnHierarchyFileLoad(const Value: TOPPGuideOnLoadFileName);
    procedure OnHierarchyFilenameGet(const Value: TOPPGuideOnFilenameSet);
    procedure OnHierarchyFileSave(const Value: TOPPGuideOnFilenameWrite);

    procedure SetDefaultHierarchyFilename(const AFileName: String);
    procedure ClearHierarchyFilenameRecentList();
    procedure OnHierarchyFilenameRecentListLoad(const Value: TOPPGuideOnLoadStringList);
  end;

  TOPPGuideSettings = class(TInterfacedObject, IOPPGuideSettings)
  private
    fData: TOPPGuideSettingsData;
    fOnFormFrameLoad: TOPPGuideOnFormFrameLoad;
    fOnFormFrameSave: TOPPGuideOnFormFrameSave;
    fOnDockingFileLoad: TOPPGuideOnLoadFileName;
    fOnDockingFilenameGet: TOPPGuideOnFilenameSet;
    fOnDockingFileSave: TOPPGuideOnFilenameWrite;
    fOnHierarchyFileLoad: TOPPGuideOnLoadFileName;
    fOnHierarchyFilenameGet: TOPPGuideOnFilenameSet;
    fOnHierarchyFileSave: TOPPGuideOnFilenameWrite;
    fOnHierarchyFilenameRecentListLoad: TOPPGuideOnLoadStringList;
    procedure Save;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;

    procedure OnFormFrameLoad(const Value: TOPPGuideOnFormFrameLoad);
    procedure OnFormFrameSave(const Value: TOPPGuideOnFormFrameSave);

    procedure OnHierarchyFileLoad(const Value: TOPPGuideOnLoadFileName);
    procedure OnHierarchyFilenameGet(const Value: TOPPGuideOnFilenameSet);
    procedure OnHierarchyFileSave(const Value: TOPPGuideOnFilenameWrite);

    procedure OnDockingFileLoad(const Value: TOPPGuideOnLoadFileName);
    procedure OnDockingFilenameGet(const Value: TOPPGuideOnFilenameSet);
    procedure OnDockingFileSave(const Value: TOPPGuideOnFilenameWrite);

    procedure SetDefaultHierarchyFilename(const AFileName: String);
    procedure ClearHierarchyFilenameRecentList();
    procedure OnHierarchyFilenameRecentListLoad(const Value: TOPPGuideOnLoadStringList);
  end;

  TOPPGuideSettingsData = class(TOPPCodable)
  private
    fDockLayoutFileName: String;
    fFormFrame: TRect;
    fHierarchyFileName: String;
    // fHierarchyFileNameRecentList: TStringList;
  public

    class procedure Save(AFileName: String; AData: TOPPGuideSettingsData);
    class procedure Load(AFileName: String; out AData: TOPPGuideSettingsData);

    constructor Create;
    destructor Destroy; override;

    property DockLayoutFileName: String read fDockLayoutFileName write fDockLayoutFileName;
    property FormFrame: TRect read fFormFrame write fFormFrame;
    property HierarchyFileName: String read fHierarchyFileName write fHierarchyFileName;
    // property HierarchyFileNameRecentList:TStringList read fHierarchyFileNameRecentList write fHierarchyFileNameRecentList;
  end;

implementation

uses
  System.IOUtils;

const
  kContext = 'OPPGuide';
  SOPPGuideSettingsFileName = 'OPPGuide.settings';

  { TOPPGuideSettingsData }

constructor TOPPGuideSettingsData.Create;
begin
  fDockLayoutFileName := 'opp.guide.docking.ini';
  fHierarchyFileName := 'opp.guide.xml';
  fFormFrame := TRect.Create(TPoint.Create(0, 0), 1400, 1100);
  // fHierarchyFileNameRecentList := TStringList.Create;
end;

destructor TOPPGuideSettingsData.Destroy;
begin
  // fHierarchyFileNameRecentList.Free;
  inherited;
end;

class procedure TOPPGuideSettingsData.Load(AFileName: String; out AData: TOPPGuideSettingsData);
begin
  try
    TOPPCodableHelper<TOPPGuideSettingsData>.Decode(AFileName, AData);
  except
    on E: Exception do
    begin
      AData := nil;
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPGuideSettingsData.Save(AFileName: String; AData: TOPPGuideSettingsData);
begin
  try
    TOPPCodableHelper<TOPPGuideSettingsData>.Encode(AFileName, AData);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

{ TOPPGuideSettings }

procedure TOPPGuideSettings.ClearHierarchyFilenameRecentList;
begin
  //
end;

constructor TOPPGuideSettings.Create;
begin
end;

destructor TOPPGuideSettings.Destroy;
begin
  Save;
  inherited;
end;

procedure TOPPGuideSettings.OnDockingFileLoad(const Value: TOPPGuideOnLoadFileName);
begin
  fOnDockingFileLoad := Value;
end;

procedure TOPPGuideSettings.OnDockingFilenameGet(const Value: TOPPGuideOnFilenameSet);
begin
  fOnDockingFilenameGet := Value;
end;

procedure TOPPGuideSettings.OnDockingFileSave(const Value: TOPPGuideOnFilenameWrite);
begin
  fOnDockingFileSave := Value;
end;

procedure TOPPGuideSettings.OnFormFrameLoad(const Value: TOPPGuideOnFormFrameLoad);
begin
  fOnFormFrameLoad := Value;
end;

procedure TOPPGuideSettings.OnFormFrameSave(const Value: TOPPGuideOnFormFrameSave);
begin
  fOnFormFrameSave := Value;
end;

procedure TOPPGuideSettings.OnHierarchyFileLoad(const Value: TOPPGuideOnLoadFileName);
begin
  fOnHierarchyFileLoad := Value;
end;

procedure TOPPGuideSettings.OnHierarchyFilenameGet(const Value: TOPPGuideOnFilenameSet);
begin
  fOnHierarchyFilenameGet := Value;
end;

procedure TOPPGuideSettings.OnHierarchyFilenameRecentListLoad(const Value: TOPPGuideOnLoadStringList);
begin
  fOnHierarchyFilenameRecentListLoad := Value;
end;

procedure TOPPGuideSettings.OnHierarchyFileSave(const Value: TOPPGuideOnFilenameWrite);
begin
  fOnHierarchyFileSave := Value;
end;

procedure TOPPGuideSettings.Load;
var
  fResult: TOPPGuideSettingsData;
  fFilePath: String;
begin
  fFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SOPPGuideSettingsFileName);
  if not TFile.Exists(fFilePath) then
  begin
    fData := TOPPGuideSettingsData.Create;
  end else begin
    TOPPGuideSettingsData.Load(SOPPGuideSettingsFileName, fData);
  end;

  { form frame }
  if Assigned(fOnFormFrameLoad) then
    fOnFormFrameLoad(fData.FormFrame);

  { dock layout }
  if Assigned(fOnDockingFileLoad) then
  begin
    if Assigned(fOnDockingFilenameGet) then
    begin
      fData.DockLayoutFileName := fOnDockingFilenameGet;
    end;
    fOnDockingFileLoad(fData.DockLayoutFileName);
  end;

  { hierarchy datafile }
  if Assigned(fOnHierarchyFileLoad) then
  begin
    if Length(fData.HierarchyFileName) = 0 then
    begin
      if Assigned(fOnHierarchyFilenameGet) then
      begin
        fData.HierarchyFileName := fOnHierarchyFilenameGet;
      end;
    end;
    fOnHierarchyFileLoad(fData.HierarchyFileName);
  end;
end;

procedure TOPPGuideSettings.SetDefaultHierarchyFilename(const AFileName: String);
begin
  fData.HierarchyFileName := AFileName;
end;

procedure TOPPGuideSettings.Save;
var
  fFilePath: String;
begin
  fFilePath := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(SOPPGuideSettingsFileName);
  if TFile.Exists(fFilePath) then
  begin
    try
      TFile.Delete(fFilePath);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
        raise E;
      end;
    end;
  end;

  if Assigned(fOnFormFrameSave) then
    fData.FormFrame := fOnFormFrameSave;

  if Assigned(fOnDockingFileSave) then
  begin
    if Assigned(fOnDockingFilenameGet) then
    begin
      fData.DockLayoutFileName := fOnDockingFilenameGet;
    end;
    fOnDockingFileSave(fData.DockLayoutFileName);
  end;

  if Assigned(fOnHierarchyFileSave) then
  begin
    if Length(fData.HierarchyFileName) = 0 then
    begin
      if Assigned(fOnHierarchyFilenameGet) then
      begin
        fData.HierarchyFileName := fOnHierarchyFilenameGet;
      end;
    end;
    fOnHierarchyFileSave(fData.HierarchyFileName);
  end;

  TOPPGuideSettingsData.Save(SOPPGuideSettingsFileName, fData);
end;

end.
