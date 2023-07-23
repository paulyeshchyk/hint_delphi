unit OPP_Guide_API_Dataprovider;

interface

uses
  System.Classes,
  Vcl.Controls,
  Datasnap.DBClient,
  OPP_Guide_API_Object_Converter,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API;

type

  TOPPDatasetToObjectCallback = reference to function(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;

  TOPPGuideAPIDataprovider = class(TControl, IOPPGuideAPIDataprovider)
  private
    fClientDataset: TClientDataset;
    fObjectConverter: IOPPGuideObjectConverter;
    procedure SetObjectConverter(const Value: IOPPGuideObjectConverter);
    function GetObjectConverter: IOPPGuideObjectConverter;
  public
    constructor Create(AOwner: TComponent); override;

    function GetDataset: TClientDataset;
    function GetStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function GetParentStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function Add(): IOPPGuideAPIIdentifiable;
    function AddChild(const AParentIdentifier: String): IOPPGuideAPIIdentifiable;
    function ActiveItem: IOPPGuideAPIIdentifiable;
    function ActiveItemSubscCount: Integer;
    function BuildFilter(fieldName, pident: Variant): String;
    procedure ListOfNodes(AStartFrom: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; ACompletion: TOPPGuideChainOnAddItem);
    procedure GetScriptedStream(AObject: IOPPGuideAPIIdentifiable; completion: TOPPBlobToStreamCompletion);

    procedure SaveToFile(const AFilename: String);
    procedure LoadFromFile(const AFilename: String);
    procedure EmptyDataset;

    property ClientDataset: TClientDataset read GetDataset write fClientDataset;
    [weak]
    property ObjectConverter: IOPPGuideObjectConverter read GetObjectConverter write SetObjectConverter;
  end;

implementation

uses
  // remove asap
  OPP_Guide_API_Context_Step,

  Data.DB,
  OPP.Help.Log,
  Variants,
  System.SysUtils, System.Generics.Collections;

type
  TOPPGuideAPIStepFilterType = record helper for TOPPGuideAPIIdentifiableFilterType
    class function PIdentifier(AIdentifier: String): TOPPGuideAPIIdentifiableFilterType; static;
  end;

  TOPPBlobToStreamCompletion2 = reference to procedure(AStream: TStream);

  TOPPClientDataSetHelper = class helper for TDataSet
    procedure BlobToStream(AFieldName: String; completion: TOPPBlobToStreamCompletion2); overload;
  end;

const
  kContext: String = 'GuideAPIProvider';

  { TOPPGuideAPIDataprovider }

function TOPPGuideAPIDataprovider.ActiveItem: IOPPGuideAPIIdentifiable;
begin
  result := fObjectConverter.GetObjectFromDataset(fClientDataset);
end;

function TOPPGuideAPIDataprovider.ActiveItemSubscCount: Integer;
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  result := 0;
  fObject := fObjectConverter.GetObjectFromDataset(fClientDataset);
  if (not Assigned(fObject)) then
    exit;
  result := fObjectConverter.DescendantsCount(fClientDataset, fObject.IdentifierValue);
end;

function TOPPGuideAPIDataprovider.Add: IOPPGuideAPIIdentifiable;
begin
  result := AddChild('');
end;

function TOPPGuideAPIDataprovider.AddChild(const AParentIdentifier: String): IOPPGuideAPIIdentifiable;
begin
  try
    result := fObjectConverter.NewObject(fClientDataset, AParentIdentifier);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

constructor TOPPGuideAPIDataprovider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fClientDataset := nil;
end;

procedure TOPPGuideAPIDataprovider.EmptyDataset;
begin
  if not Assigned(fClientDataset) then
    exit;
  fClientDataset.DisableControls;
  try
    fClientDataset.EmptyDataset;
  finally
    fClientDataset.EnableControls;
  end;
end;

function TOPPGuideAPIDataprovider.GetDataset: TClientDataset;
begin
  result := fClientDataset;
end;

function TOPPGuideAPIDataprovider.GetObjectConverter: IOPPGuideObjectConverter;
begin
  result := fObjectConverter;
end;

procedure TOPPGuideAPIDataprovider.SetObjectConverter(const Value: IOPPGuideObjectConverter);
begin
  fObjectConverter := Value;
end;

function TOPPGuideAPIDataprovider.GetParentStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
var
  fFilter: String;
  cloned: TClientDataset;
  fPIdentifier: String;
  fList: TOPPGuideAPIIdentifiableList;
  fItem: IOPPGuideAPIIdentifiable;
begin
  result := nil;

  if not(Assigned(fClientDataset)) and (not fClientDataset.Active) then
    exit;

  if (Length(AIdentifier) = 0) then
  begin
    result := nil;
    exit;
  end;

  fFilter := fObjectConverter.FilterForIdentifier(AIdentifier);

  fList := fObjectConverter.GetObjectsFromDataset(fClientDataset, fFilter);
  if not(Assigned(fList) and (fList.Count = 1)) then
    exit;
  fItem := fList.First;
  result := GetStepByIdentifier(fItem.PIdentifierValue);
end;

procedure TOPPGuideAPIDataprovider.GetScriptedStream(AObject: IOPPGuideAPIIdentifiable; completion: TOPPBlobToStreamCompletion);
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

  if ((VarIsNull(fIdent)) or (VarIsEmpty(fIdent))) then
  begin
    if Assigned(completion) then
      completion(nil, nil);
    exit;
  end;

  fFilter := self.BuildFilter(fIdentName, fIdent);
  fCDS := TClientDataset.Create(nil);
  try
    fCDS.CloneCursor(self.GetDataset, false);
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

function TOPPGuideAPIDataprovider.GetStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
var
  fFilter: String;
  fList: TOPPGuideAPIIdentifiableList;
begin
  result := nil;

  if not(Assigned(fClientDataset)) and (not fClientDataset.Active) then
    exit;

  if (Length(AIdentifier) = 0) then
  begin
    exit;
  end;

  fFilter := fObjectConverter.FilterForIdentifier(AIdentifier);
  fList := fObjectConverter.GetObjectsFromDataset(fClientDataset, fFilter);
  if not Assigned(fList) then
  begin
    exit;
  end;

  if (fList.Count <> 1) then
  begin
    exit;
  end;

  result := fList.First;
end;

procedure TOPPGuideAPIDataprovider.ListOfNodes(AStartFrom: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; ACompletion: TOPPGuideChainOnAddItem);
var
  fSubsFilter: String;
  fChild: IOPPGuideAPIIdentifiable;
  fChildrenList: TList<IOPPGuideAPIIdentifiable>;
begin
  if not Assigned(AStartFrom) then
    exit;
  case ADirection of
    ndNodeOnly:
      begin
        if Assigned(ACompletion) then
          ACompletion(AStartFrom);
      end;
    ndFromNodeToChildren:
      begin
        if Assigned(ACompletion) then
          ACompletion(AStartFrom);

        fSubsFilter := self.BuildFilter(AStartFrom.PIdentifierName, AStartFrom.IdentifierValue);
        fChildrenList := self.GetObjectConverter.GetObjectsFromDataset(self.GetDataset, fSubsFilter);

        for fChild in fChildrenList do
        begin
          self.ListOfNodes(fChild, ADirection, ACompletion);
        end;
      end;
    ndFromNodeToParent:
      begin
        fSubsFilter := self.BuildFilter(AStartFrom.IdentifierName, AStartFrom.PIdentifierValue);
        fChildrenList := self.GetObjectConverter.GetObjectsFromDataset(Self.GetDataset, fSubsFilter);
        for fChild in fChildrenList do
        begin
          self.ListOfNodes(fChild, ADirection, ACompletion);
        end;
        if Assigned(ACompletion) then
          ACompletion(AStartFrom);
      end;
  end;
end;

procedure TOPPGuideAPIDataprovider.LoadFromFile(const AFilename: String);
begin
  if not Assigned(fClientDataset) then
    exit;
  fClientDataset.LoadFromFile(AFilename);
end;

procedure TOPPGuideAPIDataprovider.SaveToFile(const AFilename: String);
begin
  if not Assigned(fClientDataset) then
    exit;
  fClientDataset.SaveToFile(AFilename, dfXMLUTF8);
end;

function TOPPGuideAPIDataprovider.BuildFilter(fieldName, pident: Variant): String;
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
    result := Format('%s LIKE ''%s''', [VarToStr(fieldName), VarToStr(pident)]);
  end;
end;

{ TOPPGuideAPIStepFilterType }

class function TOPPGuideAPIStepFilterType.PIdentifier(AIdentifier: String): TOPPGuideAPIIdentifiableFilterType;
begin
  TOPPGuideAPIIdentifiableFilterType.Filter('PIdentifier', AIdentifier)
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

end.
