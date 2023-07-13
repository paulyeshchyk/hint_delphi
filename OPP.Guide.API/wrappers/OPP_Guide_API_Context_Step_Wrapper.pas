unit OPP_Guide_API_Context_Step_Wrapper;

interface

uses
  Datasnap.DBClient,
  OPP_Guide_API_Context_Step;

type
  TOPPGuideAPIContextStepWrapper = class helper for TOPPGuideAPIContextStep
  public
    class function WrapFromDataset(ADataset: TClientDataset): TOPPGuideAPIContextStep; static;
  end;

implementation

{ TOPPGuideAPIContextStepWrapper }

class function TOPPGuideAPIContextStepWrapper.WrapFromDataset(ADataset: TClientDataset): TOPPGuideAPIContextStep;
var
  fCDS: TClientDataset;
begin
  result := nil;
  if not assigned(ADataset) then
    exit;

  fCDS := TClientDataset.Create(nil);
  try
    fCDS.CloneCursor(ADataset, false);
    result := TOPPGuideAPIContextStep.Create;
    result.NodeType := ADataset.FieldByName('NodeType').AsString;
    result.Caption := ADataset.FieldByName('Caption').AsString;
    result.ActionIdentifier := ADataset.FieldByName('ActionIdentifier').AsString;
    result.Identifier := ADataset.FieldByName('Identifier').AsString;
  finally
    fCDS.Free;
  end;

end;

end.
