unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  System.Classes;

type
  TOPPGuideCompletion = reference to procedure(ALog: String);

  TOPPGuideExecutor = class
  private

  public
    class function run(dataset: TClientDataSet; ident: variant; runSubs: Boolean; completion: TOPPGuideCompletion): Boolean;
    class function runSubs(dataset: TClientDataSet; pident: variant; completion: TOPPGuideCompletion): Boolean;
  end;

  TOPPClientDataSetHelper = class helper for TClientDataSet
    procedure BlobToStream(AFieldName: String; const AStream: TStream);
  end;

implementation

uses
  System.SysUtils, Variants,
  Vcl.Forms,
  WinAPI.ShellAPI, WinAPI.Windows,

  OPP.Help.System.Messaging;

{ TOPPGuideExecutor }

class function TOPPGuideExecutor.run(dataset: TClientDataSet; ident: variant; runSubs: Boolean; completion: TOPPGuideCompletion): Boolean;
var
  fCDS: TClientDataSet;
  fCaption: String;
  fStream: TMemoryStream;
  fScriptSize: Integer;
  fScript: PWideChar;//UTF8String;
  ss: TStringStream;
begin
  result := false;
  if not Assigned(dataset) then
    exit;
  fCDS := TClientDataSet.Create(nil);
  try
    fCDS.CloneCursor(dataset, false);
    if VarIsNull(ident) or VarIsEmpty(ident) then
    begin
      fCDS.Filter := '';
    end else begin
      fCDS.Filter := Format('identifier LIKE ''%s''', [ident]);
    end;
    fCDS.Filtered := true;
    fCDS.First;
    if fCDS.Eof then
    begin
      if Assigned(completion) then
        completion('nothing to run');
    end else begin
      while not fCDS.Eof do
      begin
        if Assigned(completion) then
          completion(Format('Started: %s', [fCDS.FieldByName('Caption').AsString]));
        { --- }

        fStream := TMemoryStream.Create;
        try
          fCDS.BlobToStream('Script', fStream);
          fStream.Position := 0;
          fStream.Read(fScriptSize, SizeOf(fScriptSize));

          ss := TStringStream.Create;
          try
            ss.CopyFrom(fStream, fScriptSize);
            fScript := PWideChar(ss.DataString);
          finally
            ss.Free;
          end;

        //cmd.exe /C start mailto:test@test.com?subject=A
        //cmd.exe /C start D:\Compiled\Executable\OPPHelpPreview.exe
        //cmd.exe /C D:\Compiled\Executable\OPPHelpPreview.exe
        //D:\Compiled\Executable\OPPHelpPreview.exe
        //rundll32.exe user32.dll,SendMessage 65535 0 0 "Test"

        TOPPSystemMessageHelper.RunScript(fScript, Application.handle, 100,
          procedure(ARunResultType: Exception)
          begin
          end);

        finally
          fStream.Free;
        end;


        { --- }
        if Assigned(completion) then
          completion(Format('Finished: %s', [fCDS.FieldByName('Caption').AsString]));

        if runSubs then
          TOPPGuideExecutor.runSubs(dataset, fCDS.FieldByName('identifier').value, completion);
        fCDS.Next;
      end;
    end;
  finally
    fCDS.Free;
  end;
end;

class function TOPPGuideExecutor.runSubs(dataset: TClientDataSet; pident: variant; completion: TOPPGuideCompletion): Boolean;
var
  fCDS: TClientDataSet;
begin
  result := false;
  if not Assigned(dataset) then
    exit;
  if VarIsNull(pident) or VarIsEmpty(pident) then
  begin
    if Assigned(completion) then
      completion('nothing to run:[pident is null]');
    exit;
  end;
  fCDS := TClientDataSet.Create(nil);
  try
    fCDS.CloneCursor(dataset, false);
    if VarIsNull(pident) or VarIsEmpty(pident) then
    begin
      fCDS.Filter := '';
    end else begin
      fCDS.Filter := Format('pidentifier LIKE ''%s''', [pident]);
    end;
    fCDS.Filtered := true;
    fCDS.IndexFieldNames := 'Order';

    fCDS.First;
    while not fCDS.Eof do
    begin
      TOPPGuideExecutor.run(dataset, fCDS.FieldByName('identifier').value, true, completion);
      fCDS.Next;
    end;
  finally
    fCDS.Free;
  end;
end;

{ TOPPClientDataSetHelper }

procedure TOPPClientDataSetHelper.BlobToStream(AFieldName: String; const AStream: TStream);
var
  fField: TField;
  pBytes: TArray<Byte>;
  fDataSize: Integer;
begin
  if not Assigned(AStream) then
    exit;
  fField := Fields.FieldByName(AFieldName);
  if not Assigned(fField) then
    exit;
  pBytes := fField.AsBytes;

  fDataSize := Length(pBytes);
  AStream.Write(fDataSize, SizeOf(fDataSize));

  AStream.Write(pBytes, Length(pBytes));
end;

end.
