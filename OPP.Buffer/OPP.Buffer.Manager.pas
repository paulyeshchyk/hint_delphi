unit OPP.Buffer.Manager;

interface

uses
  System.Classes,
  Vcl.Clipbrd,
  Datasnap.dbclient, Data.DB,

  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper,

  OPP.Buffer.Manager.Settings,
  OPP.Buffer.Manager.Settings.Data,

  OPP.Buffer.Clipboard,
  OPP.Buffer.Manager.Dataset,

  System.Generics.Collections,
  System.Variants;

type

  IOPPBufferManager = interface
    procedure OnClipboardChange(Sender: TObject);
    procedure SetFormat(AFormat: TOPPBufferManagerItemFormat);
    procedure AddEmpty();
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;

    function GetDataset: IOPPBufferManagerDataset;
    function GetSettings: IOPPBufferManagerSettings;

    procedure LoadRecords();
    procedure SaveRecords();
    procedure SetRecordsStorageFileName(AFileName: String = '');

    property Dataset: IOPPBufferManagerDataset read GetDataset;
    property Settings: IOPPBufferManagerSettings read GetSettings;
  end;

  TOPPBufferManager = class(TInterfacedObject, IOPPBufferManager)
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetFormat(AFormat: TOPPBufferManagerItemFormat);
    procedure AddEmpty();
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;
  private
    fSettings: IOPPBufferManagerSettings;
    fDataset: TOPPBufferManagerDataset;
    fFormat: TOPPBufferManagerItemFormat;

    function GetDataset: IOPPBufferManagerDataset;
    function GetSettings: IOPPBufferManagerSettings;

    procedure AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
    procedure SetRecordsStorageFileName(AFileName: String = '');
    procedure LoadRecords();
    procedure SaveRecords();
    function GetRecordsStorageFileName(AFileName: String = ''): String;
    procedure SaveClipboardToManagerRecord(AFormat: Word);
    procedure OnClipboardChange(Sender: TObject);
    function GetCanAcceptRecord: Boolean;

    property Dataset: IOPPBufferManagerDataset read GetDataset;
    property Settings: IOPPBufferManagerSettings read GetSettings;
    property CanAcceptRecord: Boolean read GetCanAcceptRecord;
  end;

function oppBufferManager: IOPPBufferManager;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  System.IOUtils,
  OPP.Help.System.Files,
  OPP.Help.System.Str,
  OPP.Help.Log,
  Vcl.Dialogs,
  Vcl.Forms,
  WinAPI.Windows;

resourcestring
  SBufferManagerRecordsFileWasDamaged = 'Файл записей буфера обмена был повреждён!';
  SNotAbleToDeleteDamagedFileTemplate = 'При попытке восстановить файл записей произошла ошибка:'#13#10#13#10'%s'#13#10#13#10'Обратитесь к администратору';

const
  kContext = 'TOPPBufferManager';

type
  TOPPBufferManagerRecordStreamHelper = class helper for TStream
    function GetBufferManagerRecord(ASortIndex: Integer; AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
  end;

var
  fBufferManagerLock: TCriticalSection;
  fBufferManager: IOPPBufferManager;

function oppBufferManager: IOPPBufferManager;
begin
  fBufferManagerLock.Acquire;
  try
    if not assigned(fBufferManager) then
    begin
      fBufferManager := TOPPBufferManager.Create();
    end;
    result := fBufferManager;
  finally
    fBufferManagerLock.Release;
  end;
end;

{ TOPPBufferManager }

function TOPPBufferManager.AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
begin
  result := fDataset.AddRecord(ARecord);
end;

procedure TOPPBufferManager.AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
begin
  if AddRecord(ARecord) then
    SaveRecords();
end;

constructor TOPPBufferManager.Create;
begin
  inherited;

  fSettings := TOPPBufferManagerSettings.Create;

  SetFormat(ifText);

  fDataset := TOPPBufferManagerDataset.Create(nil);
  fDataset.Rebuild;

  LoadRecords();
end;

function TOPPBufferManager.DeleteFocused: Boolean;
begin
  result := false;
  if fDataset.RecNo = -1 then exit;
  try
    fDataset.Delete;
    result := true;
  except
    on E: Exception do begin
      eventLogger.Error(E, kContext);
    end;

  end;
end;

destructor TOPPBufferManager.Destroy;
begin
  fSettings := nil;
  fDataset.Free;
  inherited;
end;

function TOPPBufferManager.GetCanAcceptRecord: Boolean;
begin
  result := true;
  if not(Application.ActiveFormHandle = GetForegroundWindow()) then
  begin
    result := Settings.isExternalAllowed
  end;
end;

function TOPPBufferManager.GetDataset: IOPPBufferManagerDataset;
begin
  result := fDataset
end;

function TOPPBufferManager.GetRecordsStorageFileName(AFileName: String): String;
var
  fResult: String;
begin
  if TFile.Exists(AFileName) then
  begin
    result := AFileName;
    exit;
  end;

  fResult := Settings.GetCurrentFilePath;
  if TFile.Exists(fResult) then
  begin
    result := fResult;
    exit;
  end;

  result := Settings.GetDefaultFilePath;
end;

function TOPPBufferManager.GetSettings: IOPPBufferManagerSettings;
begin
  result := fSettings;
end;

procedure TOPPBufferManager.LoadRecords();
var
  fFileName: String;
begin
  fFileName := GetRecordsStorageFileName();
  if not TFile.Exists(fFileName) then
  begin
    eventLogger.Error(Format('File not found:[%s]', [fFileName]), kContext);
    exit;
  end;

  try
    fDataset.LoadFromFile(fFileName);
  except
    on E: EDBClient do
    begin
      ShowMessage(SBufferManagerRecordsFileWasDamaged);
      try
        TFile.Delete(fFileName);
        fDataset.Rebuild;
      except
        on E: Exception do
        begin
          ShowMessage(Format(SNotAbleToDeleteDamagedFileTemplate, [E.Message]));
        end;
      end;
    end;
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPBufferManager.OnClipboardChange(Sender: TObject);
begin
  if not self.CanAcceptRecord then
    exit;

  try
    SaveClipboardToManagerRecord(fFormat.WindowsClipboardFormat);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPBufferManager.SaveClipboardToManagerRecord(AFormat: Word);
var
  fRecord: TOPPBufferManagerRecord;
begin
  fRecord := Clipboard.CreateRecord(ifText);
  if fRecord = nil then
    exit;
  try
    self.AddRecordAndSave(fRecord);
  finally
    FreeAndNil(fRecord);
  end;
end;

procedure TOPPBufferManager.SaveRecords();
var
  fFileName: String;
begin
  fFileName := GetRecordsStorageFileName();

  try
    fDataset.SaveToFile(fFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;

end;

procedure TOPPBufferManager.AddEmpty;
var
  fRecord: TOPPBufferManagerRecord;
begin

  fRecord := TOPPBufferManagerRecord.Create;
  try
    self.AddRecord(fRecord);
  finally
    fRecord.Free;
  end;

end;

procedure TOPPBufferManager.SetFormat(AFormat: TOPPBufferManagerItemFormat);
begin
  //
end;

procedure TOPPBufferManager.SetRecordsStorageFileName(AFileName: String);
begin
  self.Settings.SetCurrentFilePath(AFileName);
end;

{ TOPPBufferManagerRecordStreamHelper }

function TOPPBufferManagerRecordStreamHelper.GetBufferManagerRecord(ASortIndex: Integer; AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
begin
  result.SortIndex := ASortIndex;
  result.IsFixed := false;
end;

initialization

fBufferManagerLock := TCriticalSection.Create;

finalization

fBufferManagerLock.Free;

end.
