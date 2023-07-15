﻿unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  OPP.Guide.Scripter,
  System.Classes,
  OPP_Guide_Executor,
  OPP_Guide_Executor_State,
  OPP_Guide_API;

type

  TOPPGuideExecutorRunStateHelper = record helper for TOPPGuideExecutorRunState
  public
    class function idle: TOPPGuideExecutorRunState; static;
    class function error(text: String): TOPPGuideExecutorRunState; static;
    class function started(text: String = ''): TOPPGuideExecutorRunState; static;
    class function finished(text: String = ''): TOPPGuideExecutorRunState; static;
    class function progress(text: String = ''): TOPPGuideExecutorRunState; static;
    function StateName: String;
  end;

  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable);
  TOPPBlobToStreamCompletion2 = reference to procedure(AStream: TStream);

  TOPPGuideExecutor = class
  private
    class var fExecutor: TOPPGuideExecutor;
    procedure GetScriptedStream(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPBlobToStreamCompletion);
    function BuildFilter(fieldName, pident: Variant): String;
  public
    class function shared: TOPPGuideExecutor; static;
    function compile(dataset: TClientDataset; Identifiable: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
    function run(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean; overload;
    function runSubs(dataset: TClientDataset; AFilter: String; Scripter: IOPPGuideScripter; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPExecutorStateCallback): Boolean;
  end;

  TOPPStreamHelper = class helper for TStream
    function CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; completion: TOPPExecutorStateCallback): Boolean;
    function RunScript(AScripter: IOPPGuideScripter; userInfo: IOPPGuideAPIIdentifiable; completion: TOPPExecutorStateCallback): Boolean;
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

function TOPPGuideExecutor.compile(dataset: TClientDataset; Identifiable: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  fObject := Identifiable(dataset);

  GetScriptedStream(dataset, fObject, Identifiable,
    procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable)
    begin
      if Assigned(AStream) then
        AStream.CompileScript(AScripter, userInfo, completion);
    end);
end;

procedure TOPPGuideExecutor.GetScriptedStream(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPBlobToStreamCompletion);
var
  fFilter: String;
  fCDS: TClientDataset;
  fIdent, fIdentName: String;
  Identifiable: IOPPGuideAPIIdentifiable;
begin
  Identifiable := AObject; // IdentifiableCallback(dataset);
  if not Assigned(Identifiable) then
  begin
    if Assigned(completion) then
      completion(nil, nil);
    exit;
  end;

  try

    fIdent := Identifiable.IdentifierValue;
    fIdentName := Identifiable.IdentifierName;

    if not((not Assigned(dataset)) or (VarIsNull(fIdent)) or (VarIsEmpty(fIdent))) then
    begin
      fFilter := BuildFilter(fIdentName, fIdent);
      fCDS := TClientDataset.Create(nil);
      try
        fCDS.CloneCursor(dataset, false);
        fCDS.Filter := fFilter;
        fCDS.Filtered := true;
        fCDS.BlobToStream('Script',
          procedure(AStream: TStream)
          begin
            completion(AStream, Identifiable);
          end);
      finally
        fCDS.Free;
      end;
    end;
  finally
    Identifiable := nil;
  end;
end;

function TOPPGuideExecutor.run(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
begin
  result := false;
  if not Assigned(dataset) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('Dataset is nil'));
    exit;
  end;

  GetScriptedStream(dataset, AObject, IdentifiableCallback,
    procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
    begin
      if not Assigned(AStream) then
      begin
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.error('Stream is nil'));
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
                  runSubs(dataset, fSubsFilter, AScripter, IdentifiableCallback, completion);
                end;
              end;
          end;
        end);
    end);

end;

function TOPPGuideExecutor.runSubs(dataset: TClientDataset; AFilter: String; Scripter: IOPPGuideScripter; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPExecutorStateCallback): Boolean;
var
  cloned: TClientDataset;
  fObject: IOPPGuideAPIIdentifiable;
begin
  result := false;
  if not Assigned(dataset) then
  begin
    if Assigned(completion) then
      completion(TOPPGuideExecutorRunState.error('Dataset is nil'));
    exit;
  end;
  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(dataset, false);
    cloned.Filter := AFilter;
    cloned.Filtered := true;
    cloned.IndexFieldNames := 'Order';

    cloned.First;
    while not cloned.Eof do
    begin
      fObject := IdentifiableCallback(cloned);
      self.run(dataset, fObject, IdentifiableCallback, true, Scripter, completion);
      cloned.Next;
    end;
  finally
    cloned.Free;
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

function TOPPStreamHelper.CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; completion: TOPPExecutorStateCallback): Boolean;
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

      if Assigned(completion) then
        completion(TOPPGuideExecutorRunState.started(VarToStr(userInfo)));

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.finished(Format('Finished [%s] with result %s', [VarToStr(userInfo), VarToStr(fScriptExecutionResult)])))
      except
        on E: Exception do
        begin
          if Assigned(completion) then
            completion(TOPPGuideExecutorRunState.error(Format('Finished [%s] with error: %s', [VarToStr(userInfo), E.Message])));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.error(Format('Finished [%s] with error: %s', [VarToStr(userInfo), E.Message])));
    end;
  finally
    ss.Free;
  end;

end;

function TOPPStreamHelper.RunScript(AScripter: IOPPGuideScripter; userInfo: IOPPGuideAPIIdentifiable; completion: TOPPExecutorStateCallback): Boolean;
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

      if Assigned(completion) then
        completion(TOPPGuideExecutorRunState.started(VarToStr(userInfo.IdentifierValue)));

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, userInfo);
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.finished(Format('Finished step [%s] with result %s', [userInfo.IdentifierValue, VarToStr(fScriptExecutionResult)])));

      except
        on E: Exception do
        begin
          if Assigned(completion) then
            completion(TOPPGuideExecutorRunState.error(Format('Finished step [%s] with error: %s', [userInfo.IdentifierValue, E.Message])));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(completion) then
          completion(TOPPGuideExecutorRunState.error(Format('Finished step [%s] with error: %s', [userInfo.IdentifierValue, E.Message])));
    end;
  finally
    ss.Free;
  end;

end;

{ TOPPGuideExecutorRunStateHelper }

class function TOPPGuideExecutorRunStateHelper.error(text: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvError;
  result.shortDescription := text;
end;

class function TOPPGuideExecutorRunStateHelper.finished(text: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvFinished;
  result.shortDescription := '';
end;

class function TOPPGuideExecutorRunStateHelper.idle: TOPPGuideExecutorRunState;
begin
  result.value := rsvIdle;
  result.shortDescription := '';
end;

class function TOPPGuideExecutorRunStateHelper.progress(text: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvProgress;
  result.shortDescription := text;
end;

class function TOPPGuideExecutorRunStateHelper.started(text: String): TOPPGuideExecutorRunState;
begin
  result.value := rsvStarted;
  result.shortDescription := text;
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
