unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  OPP.Guide.Scripter,
  System.Classes,
  OPP_Guide_Executor,
  OPP_Guide_Executor_State,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Object_Converter;

type

  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  public
    class function idle(identifier: String): TOPPGuideExecutorRunState; static;
    class function error(identifier: String; text: String): TOPPGuideExecutorRunState; static;
    class function started(identifier: String; text: String = ''): TOPPGuideExecutorRunState; static;
    class function finished(identifier: String; text: String = ''): TOPPGuideExecutorRunState; static;
    class function progress(identifier: String; text: String = ''): TOPPGuideExecutorRunState; static;
    function StateName: String;
  end;

  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable);
  TOPPBlobToStreamCompletion2 = reference to procedure(AStream: TStream);

  TOPPGuideExecutor = class
  private
    class var fExecutor: TOPPGuideExecutor;
    procedure GetScriptedStream(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; completion: TOPPBlobToStreamCompletion);
    function BuildFilter(fieldName, pident: Variant): String;
  public
    class function shared: TOPPGuideExecutor; static;
    function compile(ADataprovider: IOPPGuideAPIDataprovider; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
    function run(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean; overload;
    function runSubs(ADataprovider: IOPPGuideAPIDataprovider; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
  end;

  TOPPStreamHelper = class helper for TStream
    function CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
    function RunScript(AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
  end;

  TOPPClientDataSetHelper = class helper for TDataSet
    procedure BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion2); overload;
  end;

implementation

uses
  System.SysUtils, Variants,
  Vcl.Forms,
  WinAPI.ShellAPI, WinAPI.Windows,

  OPP.Help.System.Messaging;

{ TOPPGuideExecutor }

function TOPPGuideExecutor.BuildFilter(fieldName, pident: Variant): String;
begin
  if VarIsNull(fieldName) or VarIsEmpty(fieldName) then
  begin
    result := '';
    exit;
  end;

  if VarIsNull(pident) or VarIsEmpty(pident) then
  begin
    result := '';
  end else begin
    result := Format('%s LIKE ''%s''', [fieldName, pident]);
  end;
end;

function TOPPGuideExecutor.compile(ADataProvider: IOPPGuideAPIDataprovider; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  fObject := ADataProvider.GetObjectConverter.GetObjectFromDataset(ADataprovider.GetDataset);

  GetScriptedStream(ADataprovider, fObject,
    procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable)
    begin
      if Assigned(AStream) then
        AStream.CompileScript(AScripter, userInfo.IdentifierValue, completion);
    end);
end;

procedure TOPPGuideExecutor.GetScriptedStream(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; completion: TOPPBlobToStreamCompletion);
var
  fFilter: String;
  fCDS: TClientDataset;
  fIdent, fIdentName: String;
begin
  if not Assigned(AObject) then
  begin
    if Assigned(completion) then
      completion(nil, nil);
    exit;
  end;

  fIdent := AObject.IdentifierValue;
  fIdentName := AObject.IdentifierName;

  if ((not Assigned(ADataprovider)) or (VarIsNull(fIdent)) or (VarIsEmpty(fIdent))) then
  begin
    if Assigned(completion) then
      completion(nil, nil);
    exit;
  end;

  fFilter := BuildFilter(fIdentName, fIdent);
  fCDS := TClientDataset.Create(nil);
  try
    fCDS.CloneCursor(ADataprovider.GetDataset, false);
    fCDS.Filter := fFilter;
    fCDS.Filtered := true;
    fCDS.BlobToStream('Script',
      procedure(AStream: TStream)
      begin
        completion(AStream, AObject);
      end);
  finally
    fCDS.Free;
  end;

end;

function TOPPGuideExecutor.run(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
begin
  result := false;
  if not Assigned(ADataprovider) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('', 'dataprovider is nil'));
    exit;
  end;

  GetScriptedStream(ADataprovider, AObject,
    procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
    begin
      if not Assigned(AStream) then
      begin
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.error('', 'Stream is nil'));
        exit;
      end;

      AStream.RunScript(AScripter, AIdentifiable,
        procedure(AState: TOPPGuideExecutorRunState)
        var
          fSubsFilter: String;
        begin
          if Assigned(completion) then
            completion(AState);

          case AState.value of
            rsvFinished:
              begin
                if ArunSubs then
                begin
                  fSubsFilter := BuildFilter(AObject.PIdentifierName, AObject.IdentifierValue);
                  runSubs(ADataprovider, fSubsFilter, AScripter, completion);
                end;
              end;
          end;
        end);
    end);

end;

function TOPPGuideExecutor.runSubs(ADataprovider: IOPPGuideAPIDataprovider; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
var
  fObject: IOPPGuideAPIIdentifiable;
  fList: TOPPGuideAPIIdentifiableList;
begin
  result := false;
  if not Assigned(ADataprovider) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('', 'Dataprovider is nil'));
    exit;
  end;

  if not Assigned(ADataprovider.GetObjectConverter) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('', 'Objectconverter is nil'));
    exit;
  end;

  fList := ADataprovider.GetObjectConverter.GetObjectsFromDataset(ADataprovider.GetDataset, AFilter);
  if not Assigned(fList) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('', 'ObjectList is nil'));
    exit;
  end;

  for fObject in fList do
  begin
    self.run(ADataprovider, fObject, true, Scripter, completion);
  end;
end;

class function TOPPGuideExecutor.shared: TOPPGuideExecutor;
begin
  if not Assigned(fExecutor) then
    fExecutor := TOPPGuideExecutor.Create();

  result := fExecutor;
end;

{ TOPPClientDataSetHelper }

procedure TOPPClientDataSetHelper.BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion2);
var
  fField: TField;
  pBytes: TArray<Byte>;
  fDataSize: Integer;
  fStream: TStream;
begin

  if not Assigned(completion) then
    exit;

  fField := Fields.FieldByName(AFieldName);
  if not Assigned(fField) then
  begin
    completion(nil);
    exit;
  end;
  pBytes := fField.AsBytes;

  fDataSize := Length(pBytes);
  fStream := TMemoryStream.Create;
  try
    fStream.Write(fDataSize, SizeOf(fDataSize));
    fStream.Write(pBytes, Length(pBytes));
    completion(fStream);
  finally
    fStream.Free;
  end;
end;

{ TOPPStreamHelper }

function TOPPStreamHelper.CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
begin
  self.Position := 0;
  self.Read(fScriptSize, SizeOf(fScriptSize));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(self, fScriptSize);
    try

      if Assigned(ALogOutputCompletion) then
        ALogOutputCompletion(TOPPGuideExecutorRunState.started(VarToStr(userInfo)));

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        if Assigned(ALogOutputCompletion) then
          ALogOutputCompletion(TOPPGuideExecutorRunState.finished('', Format('Finished [%s] with result %s', [VarToStr(userInfo), VarToStr(fScriptExecutionResult)])))
      except
        on E: Exception do
        begin
          if Assigned(ALogOutputCompletion) then
            ALogOutputCompletion(TOPPGuideExecutorRunState.error('', Format('Finished [%s] with error: %s', [VarToStr(userInfo), E.Message])));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(ALogOutputCompletion) then
          ALogOutputCompletion(TOPPGuideExecutorRunState.error('', Format('Finished [%s] with error: %s', [VarToStr(userInfo), E.Message])));
    end;
  finally
    ss.Free;
  end;

end;

function TOPPStreamHelper.RunScript(AScripter: IOPPGuideScripter; stepInfo: IOPPGuideAPIIdentifiable; ALogOutputCompletion: TOPPExecutorStateCallback): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScriptExecutionResult: Variant;
begin
  self.Position := 0;
  self.Read(fScriptSize, SizeOf(fScriptSize));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(self, fScriptSize);
    try

      if Assigned(ALogOutputCompletion) then
        ALogOutputCompletion(TOPPGuideExecutorRunState.started(stepInfo.IdentifierValue));

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, stepInfo);
        if Assigned(ALogOutputCompletion) then
          ALogOutputCompletion(TOPPGuideExecutorRunState.finished(stepInfo.IdentifierValue, Format('Finished with result %s', [VarToStr(fScriptExecutionResult)])));

      except
        on E: Exception do
        begin
          if Assigned(ALogOutputCompletion) then
            ALogOutputCompletion(TOPPGuideExecutorRunState.error(stepInfo.IdentifierValue, Format('Finished with error: %s', [E.Message])));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(ALogOutputCompletion) then
          ALogOutputCompletion(TOPPGuideExecutorRunState.error(stepInfo.IdentifierValue, Format('Finished with error: %s', [E.Message])));
    end;
  finally
    ss.Free;
  end;

end;

{ TOPPGuideExecutorRunStateHelper }

class function TOPPGuideExecutorRunStateHelper.error(identifier: String; text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvError;
  result.userInfo := text;
end;

class function TOPPGuideExecutorRunStateHelper.finished(identifier: String; text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvFinished;
  result.userInfo := '';
end;

class function TOPPGuideExecutorRunStateHelper.idle(identifier: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvIdle;
  result.userInfo := '';
end;

class function TOPPGuideExecutorRunStateHelper.progress(identifier: String; text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvProgress;
  result.userInfo := text;
end;

class function TOPPGuideExecutorRunStateHelper.started(identifier: String; text: String): TOPPGuideExecutorRunState;
begin
  result.stepIdentifier := identifier;
  result.value := rsvStarted;
  result.userInfo := text;
end;

function TOPPGuideExecutorRunStateHelper.StateName: String;
begin
  case self.value of
    rsvIdle:
      result := 'rsvIdle';
    rsvStarted:
      result := 'rsvStarted';
    rsvProgress:
      result := 'rsvProgress';
    rsvFinished:
      result := 'rsvFinished';
    rsvError:
      result := 'rsvError';
  end;
end;

end.
