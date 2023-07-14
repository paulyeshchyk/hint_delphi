unit OPP_Guide_API_Context;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Context_Map;

type

  TOPPGuideAPIContext = class(TInterfacedObject, IOPPGuideAPIContext, IOPPGuideAPIContextStepListener)
  private
    fList: TOPPGuideAPIContextContainer;

    [weak]
    fParentContext: IOPPGuideAPIContext;
    fMap: TOPPGuideAPIContextMap;
    fDataprovider: IOPPGuideAPIDataprovider;

    function GetMap: TOPPGuideAPIContextMap;
    property map: TOPPGuideAPIContextMap read GetMap;
    class var fContext: TOPPGuideAPIContext;

  protected
    constructor Create(AParentContext: IOPPGuideAPIContext = nil);

  public

    class function shared: TOPPGuideAPIContext; static;
    destructor Destroy; override;
    procedure PushContextItem(const stepIdentifier: String; const contextItem: IOPPGuideAPIContextStep);
    function PullContextItem(const stepIdentifier: String): TOPPGuideAPIContextStepResult;

    { IOPPGuideAPIContext }
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    function GetParentStepResult(AStepIdentifier: String):TOPPGuideAPIContextStepResult;
    function GetStepResult(AStepIdentifier: String):TOPPGuideAPIContextStepResult;
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);

  end;

implementation

uses System.Variants, Dialogs;

{ TOPPGuideAPIContext }

procedure TOPPGuideAPIContext.Add(AChild: IOPPGuideAPIContext);
begin

end;

procedure TOPPGuideAPIContext.Clear;
begin
  //
end;

function TOPPGuideAPIContext.PullContextItem(const stepIdentifier: String): TOPPGuideAPIContextStepResult;
var
  fResult : ROPPGuideAPIContextStepResult;
begin
  fMap.TryGetValue(stepIdentifier, fResult);
  result := TOPPGuideAPIContextStepResult.Create(fResult);
end;

procedure TOPPGuideAPIContext.PushContextItem(const stepIdentifier: String; const contextItem: IOPPGuideAPIContextStep);
begin
  fMap.AddOrSetValue(stepIdentifier, contextItem.GetExecutionResult.theRecord);
end;

constructor TOPPGuideAPIContext.Create(AParentContext: IOPPGuideAPIContext);
begin
  inherited Create;

  fMap := TOPPGuideAPIContextMap.Create();

  fList := TOPPGuideAPIContextContainer.Create;

  fParentContext := AParentContext;
  if Assigned(fParentContext) then
    fParentContext.Add(self);
end;

destructor TOPPGuideAPIContext.Destroy;
var
  fChild: IOPPGuideAPIContext;
begin
  if Assigned(fMap) then
  begin
    fMap.Clear;
    fMap.Free;
  end;

  fList.Clear;
  fList.Free;
  if Assigned(fParentContext) then
    fParentContext.Remove(self);

  inherited;
end;

function TOPPGuideAPIContext.GetMap: TOPPGuideAPIContextMap;
begin
  if not Assigned(fMap) then
  begin
    fMap := TOPPGuideAPIContextMap.Create;
  end;
  result := fMap;
end;

function TOPPGuideAPIContext.GetParentStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
var
  fResult : ROPPGuideAPIContextStepResult;
  fParent : TOPPGuideAPIContextStep;
begin

  result := nil;
  fParent := TOPPGuideAPIContextStep(fDataprovider.GetParentStepByIdentifier(AStepIdentifier));
  if not Assigned(fParent) then
    exit;

  fMap.TryGetValue(fParent.IdentifierValue, fResult);
  result := TOPPGuideAPIContextStepResult.Create(fResult);

end;

function TOPPGuideAPIContext.GetStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
begin
  result := TOPPGuideAPIContextStepResult.Create;
  result.state := osUnknown;
end;

procedure TOPPGuideAPIContext.Remove(AChild: IOPPGuideAPIContext);
begin

end;

procedure TOPPGuideAPIContext.SetDataprovider(AValue: IOPPGuideAPIDataprovider);
begin
  fDataprovider := AValue;
end;

class function TOPPGuideAPIContext.shared: TOPPGuideAPIContext;
begin
  if not Assigned(fContext) then
    fContext := TOPPGuideAPIContext.Create(nil);
  result := fContext;
end;

initialization

finalization

end.
