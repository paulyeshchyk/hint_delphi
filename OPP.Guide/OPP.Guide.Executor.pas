unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  OPP.Guide.Scripter,
  System.Classes;

type
  TOPPGuideCompletion = reference to procedure(ALog: String);

  TOPPGuideExecutor = class
  public
    class function run(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean; overload;
    class function runSubs(dataset: TClientDataSet; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPStreamHelper = class helper for TStream
    function RunScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: Variant);

  TOPPClientDataSetHelper = class helper for TClientDataSet
    procedure BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion); overload;
  end;

implementation

uses
  System.SysUtils, Variants,
  Vcl.Forms,
  WinAPI.ShellAPI, WinAPI.Windows,

  OPP.Help.System.Messaging;

{ TOPPGuideExecutor }

class function TOPPGuideExecutor.run(dataset: TClientDataSet; ident: Variant; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
var
  fCDS: TClientDataSet;
  fFilter, fFilter2: String;
  function BuildFilter(fieldName, pident: Variant): String;
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

begin
  result := false;
  if not Assigned(dataset) then
    exit;

  fFilter := BuildFilter('identifier', ident);
  fCDS := TClientDataSet.Create(nil);
  try
    fCDS.CloneCursor(dataset, false);
    fCDS.Filter := fFilter;
    fCDS.Filtered := true;
    fCDS.BlobToStream('Script',
      procedure(fStream: TStream; userInfo: Variant)
      begin
        fStream.RunScript(AScripter, userInfo, completion)
      end);
    if ArunSubs then
    begin
      fFilter2 := BuildFilter('pidentifier', ident);
      runSubs(dataset, fFilter2, AScripter, completion);
    end;
  finally
    fCDS.Free;
  end;
end;

class function TOPPGuideExecutor.runSubs(dataset: TClientDataSet; AFilter: String; Scripter: IOPPGuideScripter; completion: TOPPGuideCompletion): Boolean;
var
  fCDS: TClientDataSet;
begin
  result := false;
  if not Assigned(dataset) then
    exit;
  fCDS := TClientDataSet.Create(nil);
  try
    fCDS.CloneCursor(dataset, false);
    fCDS.Filter := AFilter;
    fCDS.Filtered := true;
    fCDS.IndexFieldNames := 'Order';

    fCDS.First;
    while not fCDS.Eof do
    begin
      TOPPGuideExecutor.run(dataset, fCDS.FieldByName('identifier').value, true, Scripter, completion);
      fCDS.Next;
    end;
  finally
    fCDS.Free;
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

function TOPPStreamHelper.RunScript(AScripter: IOPPGuideScripter; userInfo: Variant; completion: TOPPGuideCompletion): Boolean;
var
  ss: TStringStream;
  fScriptSize: Integer;
  fScript: PWideChar;
  fScriptExecutionResult: Variant;
begin
  self.Position := 0;
  self.Read(fScriptSize, SizeOf(fScriptSize));

  ss := TStringStream.Create;
  try
    ss.CopyFrom(self, fScriptSize);
    fScript := PWideChar(ss.DataString);
    try

      if Assigned(completion) then
        completion(Format('Started: %s', [VarToStr(userInfo)]));

      { --- }
      fScriptExecutionResult := AScripter.RunScript(fScript);
      { --- }

      if Assigned(completion) then
        completion(Format('Finished: %s with result %s', [VarToStr(userInfo), VarToStr(fScriptExecutionResult)]));
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
