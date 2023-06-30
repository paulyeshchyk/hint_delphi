unit OPP.Buffer.Manager;

interface

uses
  System.Classes,
  Vcl.Clipbrd, Vcl.Controls,
  Datasnap.dbclient, Data.DB,

  IdHashMessageDigest,

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

  TClientDatasetMigrateCompletion = reference to procedure(AStatus: Boolean);

  TOPPMetadataCompare = class helper for TClientDataSet
  private
    function GetMetadataHash: String;
  protected
    function ValidateFile(AFileName: String): Boolean;
    procedure Migrate(AFileName: String; completion: TClientDatasetMigrateCompletion);
    property MetadataHash: String read GetMetadataHash;
  end;

  TOPPBufferManagerLoadStatus = (lsCompleted, lsMigrated, lsFailure);

  TOPPBufferManagerLoadResult = record
    status: TOPPBufferManagerLoadStatus;
    text: String;
    constructor Create(AStatus: TOPPBufferManagerLoadStatus; AText: String = '');
  end;

  TOPPBufferManagerLoadCompletion = reference to procedure(AResult: TOPPBufferManagerLoadResult);

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

    procedure MigrateStorage(AFileName: String; completion: TOPPBufferManagerLoadCompletion);
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
    procedure AddOPPInfoAndSave(AText: String; const AOPPInfo: TOPPBufferOPPInfo);
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function DeleteFocused(): Boolean;
    procedure LoadRecords(completion: TOPPBufferManagerLoadCompletion = nil);
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

  WinAPI.Messages,
  WinAPI.Windows;

resourcestring
  SErrorFileNotFoundTemplate = 'File not found:[%s]';
  SWarningClipboardChangedButNothingCopied = 'Clipboard changed, but nothing copied';
  SBufferManagerRecordsFileWasDamaged = 'Файл записей буфера обмена был повреждён!';
  SNotAbleToDeleteDamagedFileTemplate = 'При попытке восстановить файл записей произошла ошибка:'#13#10#13#10'%s'#13#10#13#10'Обратитесь к администратору';
  SErrorIncorrectVersion = 'Текущая версия буфера обмена не соответствует версии ГОЛЬФСТРИМ.';
  SHintVersionWasUpdated = 'Версия буфера обмена будет обновлена.';
  SFileLimitedAccess = 'Доступ к файлу буфера обмена ограничен.';
  SAdministratorSupportIsNeed = 'Обратитесь за помощью к администратору.';

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

  LoadRecords(
    procedure(AResult: TOPPBufferManagerLoadResult)
    begin
      case AResult.status of
        lsCompleted, lsMigrated:
          begin
            if Length(AResult.text) = 0 then
              exit;
            eventLogger.Flow(AResult.text, kContext);
          end;
        lsFailure:
          begin
            if Length(AResult.text) = 0 then
              exit;
            eventLogger.Error(AResult.text, kContext);
          end;
      end;
    end);
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
    fRecord.text := AText;
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

  result := fDataset.AddRecord(ARecord, fMaxAllowed, fSettings.GetAllowDuplicates);
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
  result := nil;
  if Length(fOPPInfoExtractors) = 0 then
  begin
    eventLogger.Warning('No extractor found', kContext);
    exit;
  end;

  fFound := false;
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

procedure TOPPBufferManager.LoadRecords(completion: TOPPBufferManagerLoadCompletion);
var
  fFileName: String;
begin
  fFileName := GetRecordsStorageFileName();
  if not TFile.Exists(fFileName) then
  begin
    if assigned(completion) then
      completion(TOPPBufferManagerLoadResult.Create(lsFailure, Format(SErrorFileNotFoundTemplate, [fFileName])));
    exit;
  end;

  eventLogger.Debug(Format('Load content from file: %s', [fFileName]), kContext);

  if not self.fDataset.ValidateFile(fFileName) then
  begin
    MigrateStorage(fFileName, completion);
    exit;
  end;

  try
    fDataset.LoadFromFile(fFileName);
    if assigned(completion) then
      completion(TOPPBufferManagerLoadResult.Create(lsCompleted, ''));
  except
    on E: EDBClient do
    begin
      try
        TFile.Delete(fFileName);
        fDataset.Rebuild;
        if assigned(completion) then
          completion(TOPPBufferManagerLoadResult.Create(lsCompleted, 'File was deleted'));
      except
        on E: Exception do
        begin
          if assigned(completion) then
            completion(TOPPBufferManagerLoadResult.Create(lsFailure, E.Message));
        end;
      end;
    end;
    on E: Exception do
    begin
      if assigned(completion) then
        completion(TOPPBufferManagerLoadResult.Create(lsFailure, E.Message));
    end;
  end;
end;

procedure TOPPBufferManager.MigrateStorage(AFileName: String; completion: TOPPBufferManagerLoadCompletion);
begin

  try

    TFile.Delete(AFileName);

    eventLogger.Flow('Started dataset migration', kContext);

    self.fDataset.Migrate(AFileName,
      procedure(AStatus: Boolean)
      var
        fCompletionResult: TOPPBufferManagerLoadResult;
      begin
        eventLogger.Flow('Finished dataset migration', kContext);

        try
          self.fDataset.SaveToFile(AFileName);
          fCompletionResult.status := lsMigrated;
          fCompletionResult.text := Format('%s' + #13#10 + #13#10 + '%s', [SErrorIncorrectVersion, SHintVersionWasUpdated]);
          if assigned(completion) then
            completion(fCompletionResult);
        except
          on E: Exception do
          begin
            eventLogger.Error(E, kContext);
            if assigned(completion) then
              completion(TOPPBufferManagerLoadResult.Create(lsFailure, E.Message));
          end;
        end;
      end);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
      if assigned(completion) then
        completion(TOPPBufferManagerLoadResult.Create(lsFailure, Format('%s' + #13#10 + #13#10 + '%s' + #13#10 + #13#10 + #13#10 + '%s', [SFileLimitedAccess, AFileName, SAdministratorSupportIsNeed])));
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
    eventLogger.Warning(SWarningClipboardChangedButNothingCopied, kContext);
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
    eventLogger.Warning('Extractor is not defined', kContext);
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

{ TOPPMetadataCompare }

function TOPPMetadataCompare.GetMetadataHash: String;
var
  fField: TField;
  fList: TStringList;
  fItem: String;
  fStream: TMemoryStream;
  md5: TIdHashMessageDigest5;
begin
  result := '';

  fList := TStringList.Create;
  try

    for fField in self.Fields do
    begin
      fItem := Format('%s-%d-%d', [fField.FieldName, Integer(fField.DataType), fField.DataSize]);
      fList.Add(fItem);
    end;

    fStream := TMemoryStream.Create;
    try
      fList.SaveToStream(fStream);
      fStream.Position := 0;
      md5 := TIdHashMessageDigest5.Create;
      try
        result := md5.HashStreamAsHex(fStream);
      finally
        md5.Free;
      end;
    finally
      fStream.Free;
    end;
  finally
    fList.Free;
  end;
end;

procedure TOPPMetadataCompare.Migrate(AFileName: String; completion: TClientDatasetMigrateCompletion);
var
  fFieldDestination, fFieldSource: TField;
  fSourceDataset: TClientDataSet;
begin

  fSourceDataset := TClientDataSet.Create(nil);
  try
    try
      fSourceDataset.LoadFromFile(AFileName);
      fSourceDataset.First;
      while not fSourceDataset.Eof do
      begin
        self.Insert;
        for fFieldDestination in self.Fields do
        begin
          fFieldSource := fSourceDataset.Fields.FindField(fFieldDestination.FieldName);
          if not assigned(fFieldSource) then
          begin
            eventLogger.Debug(Format('Skipping field %s', [fFieldDestination.FieldName]), Format('%s-%s', ['Import', kContext]));
            continue;
          end;

          try
            fFieldDestination.Value := fFieldSource.Value;
          except
            on E: Exception do
            begin
              eventLogger.Error(E, Format('%s-%s', ['Import', kContext]));
            end;
          end;
        end;
        self.Post;
        fSourceDataset.Next;
      end;

      if assigned(completion) then
        completion(true);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
        if assigned(completion) then
          completion(false);
      end;
    end;
  finally
    fSourceDataset.Free;
  end;
end;

function TOPPMetadataCompare.ValidateFile(AFileName: String): Boolean;
var
  ds: TClientDataSet;
  hash1, hash2: String;
begin

  result := false;
  ds := TClientDataSet.Create(nil);
  try
    try
      ds.LoadFromFile(AFileName);
      hash1 := ds.MetadataHash;
      hash2 := self.MetadataHash;
      result := (hash1 = hash2);

    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    ds.Free;
  end;
end;

{ TOPPBufferManagerLoadResult }

constructor TOPPBufferManagerLoadResult.Create(AStatus: TOPPBufferManagerLoadStatus; AText: String);
begin
  self.status := AStatus;
  self.text := AText;
end;

initialization

fBufferManagerLock := TCriticalSection.Create;

finalization

fBufferManagerLock.Free;

end.
