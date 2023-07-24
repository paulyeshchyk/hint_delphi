unit OPP.Guide.API.ObjectConverter.Step;

interface

uses
  Data.DB, Datasnap.DBClient,

  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Object_Converter;

type
  TOPPGuideAPIObjectConverterStep = class(TInterfacedObject, IOPPGuideObjectConverter)
  public
    function GetObjectFromDataset(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
    function GetObjectsFromDataset(ADataset: TClientDataset; const AFilter: String): TOPPGuideAPIIdentifiableList;
    function NewObject(ADataset: TClientDataset): IOPPGuideAPIIdentifiable; overload;
    function NewObject(ADataset: TClientDataset; AParentIdentifier: String): IOPPGuideAPIIdentifiable; overload;
    function DescendantsCount(ADataset: TClientDataset; AParentIdentifier: String): Integer;
    function FilterForIdentifier(AIdentifier: String): String;
    function FilterForPIdentifier(AIdentifier: String): String;
  end;

implementation

uses
  Variants,
  System.SysUtils,

  OPP_Guide_API_Context_Step;

{ TOPPStepGuideAPIDataProvider }

function TOPPGuideAPIObjectConverterStep.DescendantsCount(ADataset: TClientDataset; AParentIdentifier: String): Integer;
var
  fFilter: String;
  fList: TOPPGuideAPIIdentifiableList;
begin
  result := 0;

  fFilter := self.FilterForPIdentifier(AParentIdentifier);

  fList := GetObjectsFromDataset(ADataset, fFilter);
  if not Assigned(fList) then
  begin
    exit;
  end;

  result := fList.Count;
end;

function TOPPGuideAPIObjectConverterStep.FilterForIdentifier(AIdentifier: String): String;
begin
  if Length(AIdentifier) = 0 then
    result := Format('identifier IS NULL', [])
  else
    result := Format('identifier LIKE ''%s''', [AIdentifier]);
end;

function TOPPGuideAPIObjectConverterStep.FilterForPIdentifier(AIdentifier: String): String;
begin
  if Length(AIdentifier) = 0 then
    result := Format('pidentifier IS NULL', [])
  else
    result := Format('pidentifier LIKE ''%s''', [AIdentifier]);
end;

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
  fResult.IdentifierValue := ADataset.FieldByName('Identifier').AsString;
  fResult.PIdentifierValue := ADataset.FieldByName('PIdentifier').AsString;
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

function TOPPGuideAPIObjectConverterStep.NewObject(ADataset: TClientDataset; AParentIdentifier: String): IOPPGuideAPIIdentifiable;
var
  id: String;
  fGUID: TGuid;
  fDescendantsCount: Integer;
begin
  if not(Assigned(ADataset) and (ADataset.Active)) then
  begin
    result := nil;
    exit;
  end;

  fDescendantsCount := 0;
  CreateGUID(fGUID);
  id := GUIDToString(fGUID);
  ADataset.Insert;
  try
    try
      ADataset.FieldByName('identifier').Value := id;
      if Length(AParentIdentifier) = 0 then
      begin
        ADataset.FieldByName('pidentifier').Value := null;
      end else begin
        ADataset.FieldByName('pidentifier').Value := AParentIdentifier;
        fDescendantsCount := self.DescendantsCount(ADataset, AParentIdentifier);
      end;
      ADataset.FieldByName('Caption').AsString := id;
      ADataset.FieldByName('Order').AsInteger := fDescendantsCount;
      ADataset.FieldByName('NodeType').AsInteger := 0;
    except
      on E: Exception do
      begin
        ADataset.Cancel;
        raise (E);
      end;
    end;
  finally
    ADataset.Post;
  end;

end;

function TOPPGuideAPIObjectConverterStep.NewObject(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
begin
  //
end;

end.
