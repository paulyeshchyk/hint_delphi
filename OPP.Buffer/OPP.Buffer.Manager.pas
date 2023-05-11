unit OPP.Buffer.Manager;

interface

uses
  System.Classes,
  Vcl.Clipbrd, Vcl.Controls,
  Datasnap.dbclient, Data.DB,

  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper,

  OPP.Buffer.OPPInfo,
  OPP.Buffer.Manager.Dataset,
  OPP.Buffer.Manager.DatasetRecord,

  OPP.Buffer.Manager.Settings,
  OPP.Buffer.Manager.Settings.Data,

  OPP.Buffer.Clipboard,

  System.Generics.Collections,
  System.Variants, System.StrUtils;

type
  TOPPInfoExtractor = class
  protected
    function isApplicable(Sender: TWinControl): Boolean; virtual;
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo; virtual;
    procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl); virtual;
  end;

  TOPPBufferManager = class
  private
    fIgnoreClipboardMessages: Boolean;
    fDataset: TOPPBufferManagerDataset;
    fSettings: IOPPBufferManagerSettings;
    fOPPInfoExtractors: TArray<TOPPInfoExtractor>;
    procedure AddRecordAndSave(const ARecord: TOPPBufferManagerRecord);
    procedure CreateRecordAndSave(const OPPInfo: TOPPBufferOPPInfo);
    function GetCanAcceptRecord: Boolean;
    function GetDataset: IOPPBufferManagerDataset;
    function GetRecordsStorageFileName(AFileName: String = ''): String;
    function GetSettings: IOPPBufferManagerSettings;

    procedure OnCalcFields(ADataset: TDataset);
    property CanAcceptRecord: Boolean read GetCanAcceptRecord;

  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterOPPInfoExtractor(AExtractor: TOPPInfoExtractor);
    function GetOPPInfoExtractor(Sender: TWinControl): TOPPInfoExtractor;
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;

    procedure ReadDataFromControl(Sender: TWinControl);
    procedure WriteDataIntoControl(Sender: TWinControl; AData: TOPPBufferManagerRecord);

    procedure AddEmpty();
    procedure AddOPPInfoAndSave(AText: String; const AOPPInfo:TOPPBufferOPPInfo);
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;
    procedure LoadRecords();
    procedure SaveRecords(AFileName: String = '');
    procedure RemoveRecordsAfter(AAfter: Integer);
    property Settings: IOPPBufferManagerSettings read GetSettings;
    property Dataset: IOPPBufferManagerDataset read GetDataset;
    procedure SetCustomFilter(AFilter: String);
    procedure SetRecordsStorageFileName(AFileName: String = '');
  end;

function oppBufferManager: TOPPBufferManager; // IOPPBufferManager;

implementation

uses
  System.SyncObjs,
  System.SysUtils,
  System.IOUtils,
  OPP.Help.Log,
  Vcl.Dialogs,
  Vcl.Forms,

  WinAPI.Windows;

resourcestring
  SErrorFileNotFoundTemplate = 'File not found:[%s]';
  SWarningClipboardChangedButNothingCopied = 'Clipboard changed, but nothing copied';
  SBufferManagerRecordsFileWasDamaged = 'Файл записей буфера обмена был повреждён!';
  SNotAbleToDeleteDamagedFileTemplate = 'При попытке восстановить файл записей произошла ошибка:'#13#10#13#10'%s'#13#10#13#10'Обратитесь к администратору';

const
  kContext = 'TOPPBufferManager';

type
  TOPPBufferManagerRecordStreamHelper = class helper for TStream
    function GetBufferManagerRecord(ASortIndex: Integer): TOPPBufferManagerRecord;
  end;

var
  fBufferManagerLock: TCriticalSection;
  fBufferManager: TOPPBufferManager;

function oppBufferManager: TOPPBufferManager;
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
  fIgnoreClipboardMessages := false;

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

procedure TOPPBufferManager.AddOPPInfoAndSave(AText: String; const AOPPInfo: TOPPBufferOPPInfo);
var
  fRecord: TOPPBufferManagerRecord;
begin
  fRecord := TOPPBufferManagerRecord.Create;
  try
    fRecord.OPPInfo := AOPPInfo;
    fRecord.Text := AText;
    self.AddRecordAndSave(fRecord);
  finally
    fRecord.Free;
  end;
end;

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
  result := fDataset;
end;

function TOPPBufferManager.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
var
  i: Integer;
  fFound: Boolean;
  fExtractor: TOPPInfoExtractor;
begin
  fFound := false;
  result := nil;
  i := 0;

  while (not fFound) and (i < Length(fOPPInfoExtractors)) do
  begin
    fExtractor := fOPPInfoExtractors[i];
    if assigned(fExtractor) then
    begin
      result := fExtractor.GetOPPInfo(Sender);
      fFound := assigned(result);
    end;
    inc(i);
  end;

end;

function TOPPBufferManager.GetOPPInfoExtractor(Sender: TWinControl): TOPPInfoExtractor;
var
  i: Integer;
  fFound: Boolean;
  fExtractor: TOPPInfoExtractor;
begin
  fFound := false;
  result := nil;
  i := 0;

  while (not fFound) and (i < Length(fOPPInfoExtractors)) do
  begin
    fExtractor := fOPPInfoExtractors[i];
    if assigned(fExtractor) then
    begin
      fFound := fExtractor.isApplicable(Sender);
      if fFound then
        result := fExtractor;
    end;
    inc(i);
  end;
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
    eventLogger.Error(Format(SErrorFileNotFoundTemplate, [fFileName]), kContext);
    exit;
  end;

  eventLogger.Debug(Format('Load content from file: %s', [fFileName]), 'TOPPBufferManager');
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

procedure TOPPBufferManager.ReadDataFromControl(Sender: TWinControl);
var
  fOPPInfo: TOPPBufferOPPInfo;
begin
  if not self.CanAcceptRecord then
    exit;

  if fIgnoreClipboardMessages then
    exit;

  fIgnoreClipboardMessages := true;

  fOPPInfo := self.GetOPPInfo(Sender);

  if assigned(fOPPInfo) then
  begin
    try
      CreateRecordAndSave(fOPPInfo);
    finally
      fOPPInfo.Free;
    end;
  end else begin
    eventLogger.warning(SWarningClipboardChangedButNothingCopied, kContext);
  end;

  fIgnoreClipboardMessages := false;
end;

procedure TOPPBufferManager.RegisterOPPInfoExtractor(AExtractor: TOPPInfoExtractor);
var
  len: Integer;
begin
  if not assigned(AExtractor) then
    exit;

  len := Length(fOPPInfoExtractors);
  SetLength(fOPPInfoExtractors, len + 1);
  fOPPInfoExtractors[len] := AExtractor;
end;

procedure TOPPBufferManager.WriteDataIntoControl(Sender: TWinControl; AData: TOPPBufferManagerRecord);
var
  fExtractor: TOPPInfoExtractor;
begin
  if ((not assigned(AData)) or (not assigned(Sender))) then
    exit;
  fExtractor := GetOPPInfoExtractor(Sender);
  if not assigned(fExtractor) then
  begin
    eventLogger.warning('Extractor is not defined', 'TOPPBufferManager');
    exit;
  end;
  fExtractor.SetOPPInfo(AData.OPPInfo, AData.text, Sender);
end;

procedure TOPPBufferManager.RemoveRecordsAfter(AAfter: Integer);
begin
  fDataset.RemoveRecordsAfter(AAfter);
end;

procedure TOPPBufferManager.CreateRecordAndSave(const OPPInfo: TOPPBufferOPPInfo);
var
  fRecord: TOPPBufferManagerRecord;
begin

  fRecord := Clipboard.CreateRecord(OPPInfo);

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

procedure TOPPBufferManager.SetCustomFilter(AFilter: String);
begin
  fDataset.SetCustomFilter(AFilter);
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

{ TOPPInfoExtractor }

function TOPPInfoExtractor.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
begin
  result := nil;
end;

function TOPPInfoExtractor.isApplicable(Sender: TWinControl): Boolean;
begin
  result := false;
end;

procedure TOPPInfoExtractor.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
begin
  //
end;

initialization

fBufferManagerLock := TCriticalSection.Create;

finalization

fBufferManagerLock.Free;

end.
