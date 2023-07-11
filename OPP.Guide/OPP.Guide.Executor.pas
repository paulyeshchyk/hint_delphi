unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  OPP.Guide.Scripter,
  System.Classes;

type
  TOPPGuideCompletion = reference to procedure(ALog: String);
  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: Variant);

  TOPPGuideExecutor = class
  private
    class procedure GetScriptedStream(dataset: TClientDataSet; ident: Variant; completion: TOPPBlobToStreamCompletion);
    class function BuildFilter(fieldName, pident: Variant): String;
  public
    class function compile(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean; overload;
    class function run(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean; overload;
    class function runSubs(dataset: TClientDataSet; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPStreamHelper = class helper for TStream
    function CompileScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
    function RunScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPClientDataSetHelper = class helper for TDataSet
    procedure BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion); overload;
  end;

implementation

uses
  System.SysUtils, Variants,
  Vcl.Forms,
  WinAPI.ShellAPI, WinAPI.Windows,

  OPP.Help.System.Messaging;

{ TOPPGuideExecutor }

class function TOPPGuideExecutor.BuildFilter(fieldName, pident: Variant): String;
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

class function TOPPGuideExecutor.compile(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
begin
  GetScriptedStream(dataset, ident,
    procedure(AStream: TStream; userInfo: Variant)
    begin
      if Assigned(AStream) then
        AStream.CompileScript(AScripter, userInfo, completion);
    end);
end;

class procedure TOPPGuideExecutor.GetScriptedStream(dataset: TClientDataSet; ident: Variant; completion: TOPPBlobToStreamCompletion);
var
  fFilter: String;
  fCDS: TClientDataSet;
begin
  if (not Assigned(dataset)) or (VarIsNull(ident)) or (VarIsEmpty(ident)) then
  begin
    if Assigned(completion) then
      completion(nil, null);
    exit;
  end;
  fFilter := BuildFilter('identifier', ident);
  fCDS := TClientDataSet.Create(nil);
  try
    fCDS.CloneCursor(dataset, false);
    fCDS.Filter := fFilter;
    fCDS.Filtered := true;
    fCDS.BlobToStream('Script', completion);
  finally
    fCDS.Free;
  end;
end;

class function TOPPGuideExecutor.run(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
var
  fFilter2: String;

begin
  result := false;
  if not Assigned(dataset) then
    exit;

  GetScriptedStream(dataset, ident,
    procedure(AStream: TStream; userInfo: Variant)
    begin
      if Assigned(AStream) then
        AStream.RunScript(AScripter, userInfo, completion);
    end);

  if ArunSubs then
  begin
    fFilter2 := BuildFilter('pidentifier', ident);
    runSubs(dataset, fFilter2, AScripter, completion);
  end;

end;

class function TOPPGuideExecutor.runSubs(dataset: TClientDataSet; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
var
  cloned: TClientDataSet;
begin
  result := false;
  if not Assigned(dataset) then
    exit;
  cloned := TClientDataSet.Create(nil);
  try
    cloned.CloneCursor(dataset, false);
    cloned.Filter := AFilter;
    cloned.Filtered := true;
    cloned.IndexFieldNames := 'Order';

    cloned.First;
    while not cloned.Eof do
    begin
      TOPPGuideExecutor.run(dataset, cloned.FieldByName('identifier').value, true, Scripter, completion);
      cloned.Next;
    end;
  finally
    cloned.Free;
  end;
end;

{ TOPPClientDataSetHelper }

procedure TOPPClientDataSetHelper.BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion);
var
  fField: TField;
  pBytes: TArray<Byte>;
  fDataSize: Integer;
  fStream: TStream;
  fUserInfo: Variant;
begin

  if not Assigned(completion) then
    exit;

  fUserInfo := self.FieldByName('Caption').value;
  fField := Fields.FieldByName(AFieldName);
  if not Assigned(fField) then
  begin
    completion(nil, fUserInfo);
    exit;
  end;
  pBytes := fField.AsBytes;

  fDataSize := Length(pBytes);
  fStream := TMemoryStream.Create;
  try
    fStream.Write(fDataSize, SizeOf(fDataSize));
    fStream.Write(pBytes, Length(pBytes));
    completion(fStream, fUserInfo);
  finally
    fStream.Free;
  end;
end;

{ TOPPStreamHelper }

function TOPPStreamHelper.CompileScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
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
        completion(Format('Started: %s', [VarToStr(userInfo)]));

      { --- }
      try
        fScriptExecutionResult := AScripter.CompileScript(ss);
        if Assigned(completion) then
          completion(Format('Finished: %s with result %s', [VarToStr(userInfo), VarToStr(fScriptExecutionResult)]));
      except
        on E: Exception do
        begin
          if Assigned(completion) then
            completion(Format('Finished: %s with error: %s', [VarToStr(userInfo), E.Message]));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(completion) then
          completion(Format('Finished: %s with error: %s', [VarToStr(userInfo), E.Message]));
    end;
  finally
    ss.Free;
  end;

end;

function TOPPStreamHelper.RunScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
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
        completion(Format('Started: %s', [VarToStr(userInfo)]));

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss);
        if Assigned(completion) then
          completion(Format('Finished: %s with result %s', [VarToStr(userInfo), VarToStr(fScriptExecutionResult)]));
      except
        on E: Exception do
        begin
          if Assigned(completion) then
            completion(Format('Finished: %s with error: %s', [VarToStr(userInfo), E.Message]));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(completion) then
          completion(Format('Finished: %s with error: %s', [VarToStr(userInfo), E.Message]));
    end;
  finally
    ss.Free;
  end;

end;

end.
