unit OPP.Buffer.Manager.Dataset;

interface

uses
  MidasLib,
  Datasnap.dbclient, Data.DB,
  System.SysUtils,
  OPP.Buffer.Clipboard,
  OPP.Buffer.Manager.DatasetRecord;

type

  TOPPBufferManagerDatasetRecordExtractionCallback = reference to procedure(ARecord: TOPPBufferManagerRecord);

  IOPPBufferManagerDataset = interface
    procedure Rebuild;
    function AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer; ADuplicatesAllowed: Boolean): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
    function RemoveRecordsAfter(const AValue: Integer): Boolean;
    procedure RebuildSortIndex;
    procedure SetCustomFilter(AFilter: String);
    procedure ExtractRecord(callback: TOPPBufferManagerDatasetRecordExtractionCallback);
  end;

  TOPPBufferManagerDataset = class(TClientDataSet, IOPPBufferManagerDataset)
    procedure Rebuild;
    function AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer; ADuplicatesAllowed: Boolean): Boolean;
    function HasTheSameValue(const AValue: Variant): Boolean;
    function RemoveRecordsAfter(const AValue: Integer): Boolean;
    procedure RebuildSortIndex;
    procedure SetCustomFilter(AFilter: String);
    procedure ExtractRecord(callback: TOPPBufferManagerDatasetRecordExtractionCallback);
  private
    procedure DeleteRecordsAfterIndex(const AValue: Integer; const AFixed: Boolean);
  end;

  TOPPBufferManagerFieldDef = record
    name: String;
    dataType: TFieldType;
    size: Integer;
  end;

  TOPPBufferManagerRecordFields = record
    Data: TOPPBufferManagerFieldDef;
    sortIndex: TOPPBufferManagerFieldDef;
    isFixed: TOPPBufferManagerFieldDef;
    oppObject: TOPPBufferManagerFieldDef;
    loodsmanType: TOPPBufferManagerFieldDef;
    loodsmanAttr: TOPPBufferManagerFieldDef;
  end;

const
  OPPBufferManagerRecordFields: TOPPBufferManagerRecordFields = (
    { } Data: (name: 'Data'; dataType: ftString; size: 255);
    { } sortIndex: (name: 'SortIndex'; dataType: ftInteger; size: 0);
    { } isFixed: (name: 'isFixed'; dataType: ftBoolean; size: 0);
    { } oppObject: (name: 'OPPObject'; dataType: ftBlob; size: 0);
    { } loodsmanType: (name: '_TYPE'; dataType: ftString; size: 255);
    { } loodsmanAttr: (name: '_ATTR'; dataType: ftString; size: 255)
    { } );

implementation

uses
  System.Classes,
  OPP.Help.log,
  OPP.Help.System.Str,
  OPP.Help.System.JSON,
  OPP.Buffer.OPPInfo,
  Vcl.Forms;

const
  kContext = 'IOPPBufferManagerDataset';

  { TOPPBufferManagerDataset }

procedure TOPPBufferManagerDataset.Rebuild;
begin
  eventLogger.Flow('Rebuild', kContext);

  self.Close;
  self.FieldDefs.Clear;
  { data }
  with OPPBufferManagerRecordFields.Data do
    self.FieldDefs.Add(name, dataType, size);
  { sortIndex }
  with OPPBufferManagerRecordFields.sortIndex do
    self.FieldDefs.Add(name, dataType, size);
  { isFixed }
  with OPPBufferManagerRecordFields.isFixed do
    self.FieldDefs.Add(name, dataType, size);
  { oppObject }
  with OPPBufferManagerRecordFields.oppObject do
    self.FieldDefs.Add(name, dataType, size);
  { loodsmanType }
  with OPPBufferManagerRecordFields.loodsmanType do
    self.FieldDefs.Add(name, dataType, size);
  { loodsmanAttr }
  with OPPBufferManagerRecordFields.loodsmanAttr do
    self.FieldDefs.Add(name, dataType, size);
  self.CreateDataSet;
end;

procedure TOPPBufferManagerDataset.RebuildSortIndex;
var
  cloned: TOPPBufferManagerDataset;
begin
  eventLogger.Flow('RebuildSortIndex', kContext);

  cloned := TOPPBufferManagerDataset.Create(nil);
  try
    try
      cloned.CloneCursor(self, false);

      cloned.IndexFieldNames := OPPBufferManagerRecordFields.sortIndex.name;
      cloned.First;
      while (not cloned.EOF) do
      begin
        cloned.Edit;
        cloned.FieldByName(OPPBufferManagerRecordFields.sortIndex.name).AsInteger := cloned.RecNo;
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

procedure TOPPBufferManagerDataset.ExtractRecord(callback: TOPPBufferManagerDatasetRecordExtractionCallback);
var
  fBytes: TArray<Byte>;
  isUTF8: Boolean;
begin

  fBytes := self.FieldByName(OPPBufferManagerRecordFields.oppObject.name).AsBytes;
  isUTF8 := true;

  TOPPJSONParser.Deserialize<TOPPBufferOPPInfo>(fBytes, isUTF8,
    procedure(OPPInfo: TOPPBufferOPPInfo; Error: Exception)
    var
      theRecord: TOPPBufferManagerRecord;
    begin
      if not Assigned(OPPInfo) then
      begin
        eventLogger.Warning('OPPInfo is not defined', 'TOPPBufferManagerDataset');
        exit;
      end;
      if not Assigned(callback) then
      begin
        eventLogger.Warning('callback is not defined', 'TOPPBufferManagerDataset');
        exit;
      end;

      theRecord := TOPPBufferManagerRecord.Create;
      theRecord.text := self.FieldByName(OPPBufferManagerRecordFields.Data.name).AsString;
      theRecord.sortIndex := self.FieldByName(OPPBufferManagerRecordFields.sortIndex.name).AsInteger;
      theRecord.isFixed := self.FieldByName(OPPBufferManagerRecordFields.isFixed.name).AsBoolean;
      theRecord.OPPInfo := OPPInfo;
      callback(theRecord);
    end);
end;

procedure TOPPBufferManagerDataset.SetCustomFilter(AFilter: String);
begin
  eventLogger.Flow('SetCustomFilter', kContext);

  self.Filter := AFilter;
  self.Filtered := true;
end;

function TOPPBufferManagerDataset.AddRecord(const ARecord: TOPPBufferManagerRecord; AMaxAllowed: Integer; ADuplicatesAllowed: Boolean): Boolean;
begin
  eventLogger.Flow('AddRecord', kContext);

  result := false;
  if not Assigned(ARecord) then begin
    eventLogger.Warning('Can`t Add Record: it is not assigned', kContext);
    exit;
  end;

  if (HasTheSameValue(ARecord.text) and not (ADuplicatesAllowed)) then
  begin
    eventLogger.Flow('Can`t Add Record: duplicate found', kContext);
    exit;
  end;

  if self.RecordCount >= AMaxAllowed then
  begin
    eventLogger.Flow('Will remove record: Max allowed riched', kContext);
    RemoveRecordsAfter((AMaxAllowed - 1));
  end;

  try
    self.Append;
    self.FieldByName(OPPBufferManagerRecordFields.Data.name).AsVariant := ARecord.text;
    self.FieldByName(OPPBufferManagerRecordFields.sortIndex.name).AsInteger := self.RecordCount;
    self.FieldByName(OPPBufferManagerRecordFields.isFixed.name).AsBoolean := false;
    if Assigned(ARecord.OPPInfo) then
    begin
      self.FieldByName(OPPBufferManagerRecordFields.oppObject.name).AsBytes := ARecord.OPPInfo.SaveToBytes;
      self.FieldByName(OPPBufferManagerRecordFields.loodsmanType.name).AsString := ARecord.OPPInfo.loodsmanType;
      self.FieldByName(OPPBufferManagerRecordFields.loodsmanAttr.name).AsString := ARecord.OPPInfo.loodsmanAttribute;
    end;
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
  eventLogger.Flow('DeleteRecordsAfterIndex', kContext);

  cloned := TOPPBufferManagerDataset.Create(nil);
  try
    try
      cloned.CloneCursor(self, false);

      itemsLeftCount := cloned.RecordCount;

      cloned.IndexFieldNames := OPPBufferManagerRecordFields.sortIndex.name;
      cloned.First;
      while (not cloned.EOF) and (itemsLeftCount > AValue) do
      begin
        if cloned.FieldByName(OPPBufferManagerRecordFields.isFixed.name).AsBoolean = AFixed then
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
  eventLogger.Flow('RemoveRecordsAfter', kContext);

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
      cloned.Filter := Format('%s LIKE %s', [OPPBufferManagerRecordFields.Data.name, QuotedStr(AValue)]);
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
