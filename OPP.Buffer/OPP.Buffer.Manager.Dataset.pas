// MMWIN:MEMBERSCOPY
unit OPP.Buffer.Manager.Dataset;

interface

uses
  Datasnap.dbclient, Data.DB,
  System.SysUtils,
  OPP.Buffer.Clipboard;

type

  IOPPBufferManagerDataset = interface
    procedure Rebuild;
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
  end;

  TOPPBufferManagerDataset = class(TClientDataSet, IOPPBufferManagerDataset)
    procedure Rebuild;
    function AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
  end;

implementation

uses
  OPP.Help.log;

const
  kContext = 'IOPPBufferManagerDataset';

  { TOPPBufferManagerDataset }

procedure TOPPBufferManagerDataset.Rebuild;
begin
  self.Close;
  self.FieldDefs.Clear;
  self.FieldDefs.Add('Data', ftString, 255);
  self.FieldDefs.Add('SortIndex', ftInteger);
  self.FieldDefs.Add('isFixed', ftBoolean);
  self.CreateDataSet;
end;

function TOPPBufferManagerDataset.AddRecord(const ARecord: TOPPBufferManagerRecord): Boolean;
begin
  result := false;
  if not assigned(ARecord) then
    exit;

  if HasTheSameValue(ARecord.Data) then
  begin
    exit;
  end;

  try
    self.Append;
    self.FieldByName('Data').AsVariant := ARecord.Data;
    self.FieldByName('SortIndex').AsInteger := self.RecordCount;
    self.FieldByName('isFixed').AsBoolean := false;
    self.Post;
    result := true;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;

  end;
end;

function TOPPBufferManagerDataset.HasTheSameValue(const AValue: Variant): Boolean;
var
  currentPos: Integer;
begin
  currentPos := self.RecNo;
  try
    try
      self.Filter := Format('%s LIKE %s', ['Data', QuotedStr(AValue)]);
      self.Filtered := true;
      result := self.FindFirst;
      self.Filtered := false;
      self.Filter := '';
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    self.RecNo := currentPos;
  end;
end;

end.
