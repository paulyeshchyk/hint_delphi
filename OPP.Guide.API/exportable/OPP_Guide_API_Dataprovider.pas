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
    function GetStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
    function GetParentStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
    function Add(): IOPPGuideAPIContextStep;
    function AddChild(AParentIdentifier: String): IOPPGuideAPIContextStep;
    function SubsCount(AIdentifier: String): Integer;
    function ActiveItem: IOPPGuideAPIContextStep;
    function ActiveItemSubscCount: Integer;

    procedure SaveToFile(AFilename: String);
    procedure LoadFromFile(AFilename: String);
    procedure EmptyDataset;

    property ClientDataset: TClientDataset read GetDataset write fClientDataset;
    [weak]
    property ObjectConverter : IOPPGuideObjectConverter read GetObjectConverter write SetObjectConverter;
  end;

procedure Register;

implementation

uses
  // remove asap
  OPP_Guide_API_Context_Step,

  OPP.Help.Log,
  Variants,
  System.SysUtils;

const
  kContext: String = 'GuideAPIProvider';

  { TOPPGuideAPIDataprovider }

function TOPPGuideAPIDataprovider.ActiveItem: IOPPGuideAPIContextStep;
begin
  result := fObjectConverter.GetObjectFromDataset(fClientDataset) as IOPPGuideAPIContextStep;
end;

function TOPPGuideAPIDataprovider.ActiveItemSubscCount: Integer;
var
  fObject : IOPPGuideAPIIdentifiable;
begin
  result := 0;
  fObject := fObjectConverter.GetObjectFromDataset(fClientDataset);
  if (not Assigned(fObject)) then
    exit;
  result := SubsCount(fObject.IdentifierValue);
end;

function TOPPGuideAPIDataprovider.Add: IOPPGuideAPIContextStep;
begin
  result := AddChild('');
end;

function TOPPGuideAPIDataprovider.AddChild(AParentIdentifier: String): IOPPGuideAPIContextStep;
var
  id: String;
  fGUID: TGuid;
  cnt: Integer;
begin
  if not fClientDataset.Active then
  begin
    result := nil;
    exit;
  end;

  CreateGUID(fGUID);
  id := GUIDToString(fGUID);

  cnt := SubsCount(AParentIdentifier);

  fClientDataset.Insert;
  try
    try
      fClientDataset.FieldByName('identifier').Value := id;
      if Length(AParentIdentifier) = 0 then
        fClientDataset.FieldByName('pidentifier').Value := null
      else
        fClientDataset.FieldByName('pidentifier').Value := AParentIdentifier;
      fClientDataset.FieldByName('Caption').AsString := id;
      fClientDataset.FieldByName('Order').AsInteger := cnt;
      fClientDataset.FieldByName('NodeType').AsInteger := 0;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    fClientDataset.Post;
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

function TOPPGuideAPIDataprovider.GetParentStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
var
  fFilter: String;
  cloned: TClientDataset;
  fPIdentifier: String;
begin
  result := nil;

  if not(Assigned(fClientDataset)) and (not fClientDataset.Active) then
    exit;

  if (Length(AIdentifier) = 0) then
  begin
    result := nil;
    exit;
  end;

  fFilter := Format('identifier LIKE ''%s''', [AIdentifier]);

  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(fClientDataset, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    if cloned.RecordCount = 1 then
    begin
      fPIdentifier := cloned.FieldByName('pidentifier').AsString;
      result := GetStepByIdentifier(fPIdentifier);
    end;
  finally
    cloned.Free;
  end;
end;

function TOPPGuideAPIDataprovider.GetStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
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

  fFilter := Format('identifier LIKE ''%s''', [AIdentifier]);

  fList := fObjectConverter.GetObjectsFromDataset(fClientDataset, fFilter);
  if not Assigned(fList) then begin
    exit;
  end;

  if (fList.Count <> 1) then begin
    exit;
  end;

  result := fList.First as IOPPGuideAPIContextStep;
end;

procedure TOPPGuideAPIDataprovider.LoadFromFile(AFilename: String);
begin
  if not Assigned(fClientDataset) then
    exit;
  fClientDataset.LoadFromFile(AFilename);
end;

procedure TOPPGuideAPIDataprovider.SaveToFile(AFilename: String);
begin
  if not Assigned(fClientDataset) then
    exit;
  fClientDataset.SaveToFile(AFilename, dfXMLUTF8);
end;

function TOPPGuideAPIDataprovider.SubsCount(AIdentifier: String): Integer;
var
  fFilter: String;
  fList: TOPPGuideAPIIdentifiableList;
begin
  result := 0;

  if (VarIsNull(AIdentifier) or VarIsEmpty(AIdentifier)) then
  begin
    fFilter := Format('pidentifier IS NULL', []);
  end else begin
    fFilter := Format('pidentifier LIKE ''%s''', [AIdentifier]);
  end;

  fList := fObjectConverter.GetObjectsFromDataset(fClientDataset, fFilter);
  if not Assigned(fList) then begin
    exit;
  end;

  result := fList.Count;
end;

procedure Register;
begin
end;

end.
