unit OPP_Guide_API_Dataprovider;

interface

uses
  System.Classes,
  Vcl.Controls,
  Datasnap.DBClient,
  OPP_Guide_API;

type
  TOPPGuideAPIDataprovider = class(TControl, IOPPGuideAPIDataprovider)
  private
    fClientDataset: TClientDataset;
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
var
  fCDS: TClientDataset;
  fResult: TOPPGuideAPIContextStep;
begin
  result := nil;
  if not ClientDataset.Active then
    exit;

  fCDS := TClientDataset.Create(nil);
  try
    fCDS.CloneCursor(ClientDataset, false);
    fResult := TOPPGuideAPIContextStep.Create;
    fResult.NodeType := ClientDataset.FieldByName('NodeType').AsString;
    fResult.Caption := ClientDataset.FieldByName('Caption').AsString;
    fResult.Identifier := ClientDataset.FieldByName('Identifier').AsString;
    result := fResult;
  finally
    fCDS.Free;
  end;
end;

function TOPPGuideAPIDataprovider.ActiveItemSubscCount: Integer;
var
  fIdentifier: String;
begin
  result := 0;
  if (not Assigned(ClientDataset)) then
    exit;
  if (not ClientDataset.Active) then
    exit;
  fIdentifier := ClientDataset.FieldByName('Identifier').AsString;
  result := SubsCount(fIdentifier);
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
  if not ClientDataset.Active then
  begin
    result := nil;
    exit;
  end;

  CreateGUID(fGUID);
  id := GUIDToString(fGUID);

  cnt := SubsCount(AParentIdentifier);

  ClientDataset.Insert;
  try
    try
      ClientDataset.FieldByName('identifier').Value := id;
      if Length(AParentIdentifier) = 0 then
        ClientDataset.FieldByName('pidentifier').Value := null
      else
        ClientDataset.FieldByName('pidentifier').Value := AParentIdentifier;
      ClientDataset.FieldByName('Caption').AsString := id;
      ClientDataset.FieldByName('Order').AsInteger := cnt;
      ClientDataset.FieldByName('NodeType').AsInteger := 0;
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    ClientDataset.Post;
  end;
end;

constructor TOPPGuideAPIDataprovider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fClientDataset := nil;
end;

procedure TOPPGuideAPIDataprovider.EmptyDataset;
begin
  if not Assigned(self.ClientDataset) then
    exit;
  self.ClientDataset.DisableControls;
  try
    self.ClientDataset.EmptyDataSet;
  finally
    self.ClientDataset.EnableControls;
  end;

end;

function TOPPGuideAPIDataprovider.GetDataset: TClientDataset;
begin
  result := fClientDataset;
end;

function TOPPGuideAPIDataprovider.GetParentStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
var
  fFilter: String;
  cloned: TClientDataset;
  fPIdentifier: String;
begin
  result := nil;

  if not(Assigned(ClientDataset)) and (not ClientDataset.Active) then
    exit;

  if (Length(AIdentifier) = 0) then
  begin
    result := nil;
    exit;
  end;

  fFilter := Format('identifier LIKE ''%s''', [AIdentifier]);
  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(ClientDataset, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    if cloned.RecordCount = 1 then
    begin
      fPIdentifier := ClientDataset.FieldByName('pidentifier').AsString;
      result := GetStepByIdentifier(fPIdentifier);
    end;
  finally
    cloned.Free;
  end;
end;

function TOPPGuideAPIDataprovider.GetStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
var
  cloned: TClientDataset;
  fFilter: String;
  fResult: TOPPGuideAPIContextStep;
begin
  result := nil;

  if not(Assigned(ClientDataset)) and (not ClientDataset.Active) then
    exit;

  if (Length(AIdentifier) = 0) then
  begin
    result := nil;
    exit;
  end;

  fFilter := Format('identifier LIKE ''%s''', [AIdentifier]);
  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(ClientDataset, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    if cloned.RecordCount = 1 then
    begin
      fResult := TOPPGuideAPIContextStep.Create;
      fResult.NodeType := ClientDataset.FieldByName('NodeType').AsString;
      fResult.Caption := ClientDataset.FieldByName('Caption').AsString;
      fResult.Identifier := ClientDataset.FieldByName('Identifier').AsString;
      result := fResult;
    end;
  finally
    cloned.Free;
  end;

end;

procedure TOPPGuideAPIDataprovider.LoadFromFile(AFilename: String);
begin
  if not Assigned(self.ClientDataset) then
    exit;
  self.ClientDataset.LoadFromFile(AFilename);
end;

procedure TOPPGuideAPIDataprovider.SaveToFile(AFilename: String);
begin
  if not Assigned(self.ClientDataset) then
    exit;
  self.ClientDataset.SaveToFile(AFilename, dfXMLUTF8);
end;

function TOPPGuideAPIDataprovider.SubsCount(AIdentifier: String): Integer;
var
  cloned: TClientDataset;
  fFilter: String;
begin
  result := 0;

  if not(Assigned(ClientDataset)) and (not ClientDataset.Active) then
    exit;

  if (VarIsNull(AIdentifier) or VarIsEmpty(AIdentifier)) then
  begin
    fFilter := Format('pidentifier IS NULL', []);
  end else begin
    fFilter := Format('pidentifier LIKE ''%s''', [AIdentifier]);
  end;
  cloned := TClientDataset.Create(nil);
  try
    cloned.CloneCursor(ClientDataset, false);
    cloned.Filter := fFilter;
    cloned.Filtered := true;
    result := cloned.RecordCount;
  finally
    cloned.Free;
  end;

end;

procedure Register;
begin
end;

end.
