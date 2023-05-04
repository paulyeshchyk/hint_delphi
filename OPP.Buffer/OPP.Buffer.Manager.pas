unit OPP.Buffer.Manager;

interface

uses
  System.Classes,
  Vcl.Clipbrd, Vcl.Controls,
  Datasnap.dbclient, Data.DB,

  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper,

  OPP.Buffer.Manager.Settings,
  OPP.Buffer.Manager.Settings.Data,

  OPP.Buffer.Clipboard,
  OPP.Buffer.Manager.Dataset,
  OPP.Buffer.SLYK,

  System.Generics.Collections,
  System.Variants, System.StrUtils;

type

  IOPPBufferManager = interface
    procedure OnClipboardChange(Sender: TObject);
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
    fSettings: IOPPBufferManagerSettings;
    procedure AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
    function GetCanAcceptRecord: Boolean;
    function GetDataset: IOPPBufferManagerDataset;
    function GetRecordsStorageFileName(AFileName: String = ''): String;
    function GetSettings: IOPPBufferManagerSettings;
    procedure LoadRecords();
    procedure OnClipboardChange(Sender: TObject);
    procedure SaveClipboardToManagerRecord(SLYK: TOPPBufferSLYKObject);
    procedure SaveSLYKToManagerRecord(SLYK: TOPPBufferSLYKObject);
    procedure SaveRecords(AFileName: String = '');
    procedure SetRecordsStorageFileName(AFileName: String = '');
    procedure OnCalcFields(ADataset: TDataset);
    property CanAcceptRecord: Boolean read GetCanAcceptRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEmpty();
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;
    procedure RemoveRecordsAfter(AAfter: Integer);
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

  OPP.Buffer.SLYK.Extractor,

  WinAPI.Windows;

resourcestring
  SBufferManagerRecordsFileWasDamaged = '���� ������� ������ ������ ��� ��������!';
  SNotAbleToDeleteDamagedFileTemplate = '��� ������� ������������ ���� ������� ��������� ������:'#13#10#13#10'%s'#13#10#13#10'���������� � ��������������';

const
  kContext = 'TOPPBufferManager';

type
  TOPPBufferManagerRecordStreamHelper = class helper for TStream
    function GetBufferManagerRecord(ASortIndex: Integer): TOPPBufferManagerRecord;
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

  fDataset := TOPPBufferManagerDataset.Create(nil);
  fDataset.OnCalcFields := self.OnCalcFields;
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

procedure TOPPBufferManager.OnCalcFields(ADataset: TDataset);
begin

end;

procedure TOPPBufferManager.OnClipboardChange(Sender: TObject);
var
  SLYK: TOPPBufferSLYKObject;
begin
  if not self.CanAcceptRecord then
    exit;

  SLYK := TOPPBufferSLYKExtractor.GetSLYK(Sender);
  if not assigned(SLYK) then
  begin
    eventLogger.warning('Clipboard changed, but nothing copied', kContext);
    exit;
  end;

  try
    SaveClipboardToManagerRecord(SLYK);
  finally
    SLYK.Free;
  end;

end;

procedure TOPPBufferManager.RemoveRecordsAfter(AAfter: Integer);
begin
  fDataset.RemoveRecordsAfter(AAfter);
end;

procedure TOPPBufferManager.SaveSLYKToManagerRecord(SLYK: TOPPBufferSLYKObject);
var
  fRecord: TOPPBufferManagerRecord;
begin

  fRecord := TOPPBufferManagerRecord.Create;
  fRecord.SLYK := SLYK;

  try
    self.AddRecordAndSave(fRecord);
  finally
    FreeAndNil(fRecord);
  end;
end;

procedure TOPPBufferManager.SaveClipboardToManagerRecord(SLYK: TOPPBufferSLYKObject);
var
  fRecord: TOPPBufferManagerRecord;
begin

  fRecord := Clipboard.CreateRecord(SLYK);

  if fRecord = nil then
    exit;
  try
    try
      self.AddRecordAndSave(fRecord);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;

  finally
    FreeAndNil(fRecord);
  end;
end;

procedure TOPPBufferManager.SaveRecords(AFileName: String);
var
  fFileName: String;
begin
  fFileName := IfThen(Length(AFileName) = 0, GetRecordsStorageFileName(), AFileName);
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

procedure TOPPBufferManager.SetRecordsStorageFileName(AFileName: String);
begin
  self.Settings.SetCurrentFilePath(AFileName);
end;

{ TOPPBufferManagerRecordStreamHelper }

function TOPPBufferManagerRecordStreamHelper.GetBufferManagerRecord(ASortIndex: Integer): TOPPBufferManagerRecord;
begin
  result.SortIndex := ASortIndex;
  result.IsFixed := false;
end;

initialization

fBufferManagerLock := TCriticalSection.Create;

finalization

fBufferManagerLock.Free;

end.
