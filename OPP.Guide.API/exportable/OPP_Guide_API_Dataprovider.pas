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

  OPP.Help.Log,
  Variants,
  System.SysUtils;

type
  TOPPGuideAPIStepFilterType = record helper for TOPPGuideAPIIdentifiableFilterType
    class function PIdentifier(AIdentifier: String): TOPPGuideAPIIdentifiableFilterType; static;
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

{ TOPPGuideAPIStepFilterType }

class function TOPPGuideAPIStepFilterType.PIdentifier(AIdentifier: String): TOPPGuideAPIIdentifiableFilterType;
begin
  TOPPGuideAPIIdentifiableFilterType.Filter('PIdentifier',AIdentifier)
end;

end.
