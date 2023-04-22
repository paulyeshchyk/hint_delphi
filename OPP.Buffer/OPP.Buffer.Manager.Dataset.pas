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
    function AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
    function RemoveRecordsAfter(const AValue: Integer): Boolean;
    procedure RebuildSortIndex;
    procedure SetCustomFilter(AFilter: String);
  end;

  TOPPBufferManagerDataset = class(TClientDataSet, IOPPBufferManagerDataset)
    procedure Rebuild;
    function AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
    function RemoveRecordsAfter(const AValue: Integer): Boolean;
    procedure RebuildSortIndex;
    procedure SetCustomFilter(AFilter: String);
  private
    procedure DeleteRecordsAfterIndex(const AValue: Integer; const AFixed: Boolean);
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

procedure TOPPBufferManagerDataset.RebuildSortIndex;
var
  cloned: TOPPBufferManagerDataset;
begin
  cloned := TOPPBufferManagerDataset.Create(nil);
  try
    try
      cloned.CloneCursor(self, false);

      cloned.IndexFieldNames := 'SortIndex';
      cloned.First;
      while (not cloned.EOF) do
      begin
        cloned.Edit;
        cloned.FieldByName('SortIndex').AsInteger := cloned.RecNo;
        cloned.Post;
        cloned.Next;
      end;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;

  finally
    cloned.Free;
  end;

end;

procedure TOPPBufferManagerDataset.SetCustomFilter(AFilter: String);
begin
  self.Filter := AFilter;
  self.Filtered := true;
end;

function TOPPBufferManagerDataset.AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer): Boolean;
begin
  result := false;
  if not assigned(ARecord) then
    exit;

  if HasTheSameValue(ARecord.Data) then
  begin
    exit;
  end;

  if self.RecordCount >= AMaxAllowed then begin
    RemoveRecordsAfter( (AMaxAllowed - 1) );
  end;

  try
    self.Append;
    self.FieldByName('Data').AsVariant := ARecord.Data;
    self.FieldByName('SortIndex').AsInteger := self.RecordCount;
    self.FieldByName('isFixed').AsBoolean := false;
    self.Post;

    RebuildSortIndex;

    result := true;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPBufferManagerDataset.DeleteRecordsAfterIndex(const AValue: Integer; const AFixed: Boolean);
var
  cloned: TOPPBufferManagerDataset;
  itemsLeftCount: Integer;
begin
  cloned := TOPPBufferManagerDataset.Create(nil);
  try
    try
      cloned.CloneCursor(self, false);

      itemsLeftCount := cloned.RecordCount;

      cloned.IndexFieldNames := 'SortIndex';
      cloned.First;
      while (not cloned.EOF) and (itemsLeftCount > AValue) do
      begin
        if cloned.FieldByName('isFixed').AsBoolean = AFixed then
        begin
          cloned.Delete;
          itemsLeftCount := itemsLeftCount - 1;
        end else begin
        end;
        cloned.Next;
      end;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;

  finally
    cloned.Free;
  end;
end;

function TOPPBufferManagerDataset.RemoveRecordsAfter(const AValue: Integer): Boolean;
begin
  DeleteRecordsAfterIndex(AValue, false);
  DeleteRecordsAfterIndex(AValue, true);
  RebuildSortIndex;
  result := true;
end;

function TOPPBufferManagerDataset.HasTheSameValue(const AValue: Variant): Boolean;
var
  cloned: TOPPBufferManagerDataset;
begin
  result := false;
  cloned := TOPPBufferManagerDataset.Create(nil);
  try
    try
      cloned.CloneCursor(self, false);
      cloned.Filter := Format('%s LIKE %s', ['Data', QuotedStr(AValue)]);
      cloned.Filtered := true;
      result := cloned.FindFirst;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;

  finally
    cloned.Free;
  end;
end;

end.
