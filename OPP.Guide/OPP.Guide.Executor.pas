unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  OPP.Guide.Scripter,
  System.Classes,
  OPP_Guide_API;

type
  TOPPGuideCompletion = reference to procedure(ALog: String);
  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable);
  TOPPBlobToStreamCompletion2 = reference to procedure(AStream: TStream);
  TOPPIdentifiableClone = reference to function(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;

  TOPPGuideExecutor = class
  private
    class procedure GetScriptedStream(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPBlobToStreamCompletion);
    class function BuildFilter(fieldName, pident: Variant): String;
  public
    class function compile(dataset: TClientDataset; Identifiable: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean; overload;
    class function run(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean; overload;
    class function runSubs(dataset: TClientDataset; AFilter: String; Scripter: IOPPGuideScripter; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPStreamHelper = class helper for TStream
    function CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; completion: TOPPGuideCompletion): Boolean;
    function RunScript(AScripter: IOPPGuideScripter; userInfo: IOPPGuideAPIIdentifiable; completion: TOPPGuideCompletion): Boolean;
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

class function TOPPGuideExecutor.compile(dataset: TClientDataset; Identifiable: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
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

class procedure TOPPGuideExecutor.GetScriptedStream(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPBlobToStreamCompletion);
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

class function TOPPGuideExecutor.run(dataset: TClientDataset; AObject: IOPPGuideAPIIdentifiable; IdentifiableCallback: TOPPIdentifiableClone; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
var
  fFilter2: String;

begin
  result := false;
  if not Assigned(dataset) then
  begin
    if Assigned(completion) then
      completion('');
    exit;
  end;

  GetScriptedStream(dataset, AObject, IdentifiableCallback,
    procedure(AStream: TStream; AIdentifiable: IOPPGuideAPIIdentifiable)
    begin
      if Assigned(AStream) then
        AStream.RunScript(AScripter, AIdentifiable, completion);
    end);

  if ArunSubs then
  begin
    fFilter2 := BuildFilter(AObject.PIdentifierName, AObject.IdentifierValue);
    runSubs(dataset, fFilter2, AScripter, IdentifiableCallback, completion);
  end;

end;

class function TOPPGuideExecutor.runSubs(dataset: TClientDataset; AFilter: String; Scripter: IOPPGuideScripter; IdentifiableCallback: TOPPIdentifiableClone; completion: TOPPGuideCompletion): Boolean;
var
  cloned: TClientDataset;
  fObject: IOPPGuideAPIIdentifiable;
begin
  result := false;
  if not Assigned(dataset) then
    exit;
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
      TOPPGuideExecutor.run(dataset, fObject, IdentifiableCallback, true, Scripter, completion);
      cloned.Next;
    end;
  finally
    cloned.Free;
  end;
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

function TOPPStreamHelper.CompileScript(AScripter: IOPPGuideScripter; userInfo: OLEVariant; completion: TOPPGuideCompletion): Boolean;
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

function TOPPStreamHelper.RunScript(AScripter: IOPPGuideScripter; userInfo: IOPPGuideAPIIdentifiable; completion: TOPPGuideCompletion): Boolean;
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
        completion(Format('Started', ['']));

      { --- }
      try
        fScriptExecutionResult := AScripter.RunScript(ss, userInfo);
        if Assigned(completion) then
          completion(Format('Finished with result %s', [VarToStr(fScriptExecutionResult)]));
      except
        on E: Exception do
        begin
          if Assigned(completion) then
            completion(Format('Finished with error: %s', [E.Message]));
        end;
      end;
      { --- }

    except
      on E: Exception do
        if Assigned(completion) then
          completion(Format('Finished with error: %s', [E.Message]));
    end;
  finally
    ss.Free;
  end;

end;

end.
