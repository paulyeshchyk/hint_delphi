unit OPP.Guide.API.ObjectConverter.Step;

interface

uses
  Data.DB, Datasnap.DBClient,

  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Object_Converter;

type
  TOPPGuideAPIObjectConverterStep = class(TInterfacedObject, IOPPGuideObjectConverter)
  public
    function GetObjectFromDataset(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
    function GetObjectsFromDataset(ADataset: TClientDataset; const AFilter: String): TOPPGuideAPIIdentifiableList;
  end;

implementation

{ TOPPStepGuideAPIDataProvider }

function TOPPGuideAPIObjectConverterStep.GetObjectFromDataset(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
var
  fResult: TOPPGuideAPIContextStep;
begin
  result := nil;
  if not Assigned(ADataset) then
    exit;
  if not ADataset.Active then
    exit;
  fResult := TOPPGuideAPIContextStep.Create;
  fResult.NodeType := ADataset.FieldByName('NodeType').AsString;
  fResult.Caption := ADataset.FieldByName('Caption').AsString;
  fResult.Identifier := ADataset.FieldByName('Identifier').AsString;
  result := fResult;
end;

function TOPPGuideAPIObjectConverterStep.GetObjectsFromDataset(ADataset: TClientDataset; const AFilter: String): TOPPGuideAPIIdentifiableList;
var
  cloned: TClientDataset;
  fObject: IOPPGuideAPIIdentifiable;
begin
  result := TOPPGuideAPIIdentifiableList.Create;

  if not Assigned(ADataset) then
    exit;
  if not ADataset.Active then
    exit;

  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(ADataset, false);
    cloned.Filter := AFilter;
    cloned.Filtered := true;
    cloned.IndexFieldNames := 'Order';

    cloned.First;
    while not cloned.Eof do
    begin
      fObject := GetObjectFromDataset(cloned);
      result.Add(fObject);
      cloned.Next;
    end;
  finally
    cloned.Free;
  end;

end;

end.
