unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient;

type
  TOPPGuideCompletion = reference to procedure(ALog: String);

  TOPPGuideExecutor = class
    class function run(dataset: TClientDataSet; ident: variant; completion: TOPPGuideCompletion): Boolean;
    class function runSubs(dataset: TClientDataSet; pident: variant; completion: TOPPGuideCompletion): Boolean;
  end;

implementation

uses
  System.SysUtils, Variants;

{ TOPPGuideExecutor }

class function TOPPGuideExecutor.run(dataset: TClientDataSet; ident: variant; completion: TOPPGuideCompletion): Boolean;
var
  fCDS: TClientDataSet;
  fCaption: String;
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
        {---}

        {---}
        if Assigned(completion) then
          completion(Format('Finished: %s', [fCDS.FieldByName('Caption').AsString]));

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
      TOPPGuideExecutor.run(dataset, fCDS.FieldByName('identifier').value, completion);
      fCDS.Next;
    end;
  finally
    fCDS.Free;
  end;
end;

end.
