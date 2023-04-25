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
  System.Variants, System.StrUtils;

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
    procedure SaveRecords(AFileName: String = '');
    procedure SetRecordsStorageFileName(AFileName: String = '');
    procedure RemoveRecordsAfter(AAfter: Integer);

    procedure setCustomFilter(AFilter: String);

    property Dataset: IOPPBufferManagerDataset read GetDataset;
    property Settings: IOPPBufferManagerSettings read GetSettings;
  end;

  TOPPBufferManager = class(TInterfacedObject, IOPPBufferManager)
  private
    fDataset: TOPPBufferManagerDataset;
    fFormat: TOPPBufferManagerItemFormat;
    fSettings: IOPPBufferManagerSettings;
    procedure AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
    function GetCanAcceptRecord: Boolean;
    function GetDataset: IOPPBufferManagerDataset;
    function GetRecordsStorageFileName(AFileName: String = ''): String;
    function GetSettings: IOPPBufferManagerSettings;
    procedure LoadRecords();
    procedure OnClipboardChange(Sender: TObject);
    procedure SaveClipboardToManagerRecord(AFormat: Word);
    procedure SaveRecords(AFileName: String = '');
    procedure SetRecordsStorageFileName(AFileName: String = '');
    property CanAcceptRecord: Boolean read GetCanAcceptRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEmpty();
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;
    procedure RemoveRecordsAfter(AAfter: Integer);
    procedure SetFormat(AFormat: TOPPBufferManagerItemFormat);
    property Settings: IOPPBufferManagerSettings read GetSettings;
    procedure setCustomFilter(AFilter: String);
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

constructor TOPPBufferManager.Create;
begin
  inherited;

  fSettings := TOPPBufferManagerSettings.Create;

  SetFormat(ifText);

  fDataset := TOPPBufferManagerDataset.Create(nil);
  fDataset.Rebuild;

  LoadRecords();
end;

destructor TOPPBufferManager.Destroy;
begin
  fSettings := nil;
  fDataset.Free;
  inherited;
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

{ TOPPBufferManager }

function TOPPBufferManager.AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
var
  fMaxAllowed: Integer;
begin
  fMaxAllowed := fSettings.GetRecordsCountLimit;
  if not fSettings.GetUseRecordsCountLimit then
    fMaxAllowed := Integer.MaxValue;

  result := fDataset.AddRecord(ARecord, fMaxAllowed);
end;

procedure TOPPBufferManager.AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
begin
  if AddRecord(ARecord) then
    SaveRecords();
end;

function TOPPBufferManager.DeleteFocused: Boolean;
begin
  result := false;
  if fDataset.RecNo = -1 then
    exit;
  try
    fDataset.Delete;
    result := true;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;

  end;
end;

function TOPPBufferManager.GetCanAcceptRecord: Boolean;
begin
  result := true;
  if not(Application.ActiveFormHandle = GetForegroundWindow()) then
  begin
    result := Settings.GetIsExternalAllowed
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

procedure TOPPBufferManager.RemoveRecordsAfter(AAfter: Integer);
begin
  fDataset.RemoveRecordsAfter(AAfter);
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

procedure TOPPBufferManager.SaveRecords(AFileName: String);
var
  fFileName: String;
begin

  fFileName := IfThen(Length(AFileName) = 0,GetRecordsStorageFileName(),AFileName);

  try
    fDataset.SaveToFile(fFileName);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;

end;

procedure TOPPBufferManager.setCustomFilter(AFilter: String);
begin
  fDataset.setCustomFilter(AFilter);
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
