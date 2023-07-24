unit OPPGuideAPIContext;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Context,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Context_Step_Result,
  OPP_Guide_API_Dataprovider,
  OPPGuideAPIContextContainer;

type

  TOPPGuideAPIContext = class(TInterfacedObject, IOPPGuideAPIContext)
  private
  class var
    fContext: TOPPGuideAPIContext;
    fList: TOPPGuideAPIContextContainer;

    [weak]
    fParentContext: IOPPGuideAPIContext;
    fMap: TOPPGuideAPIContextMap;
    fDataprovider: IOPPGuideAPIDataprovider;

    function GetMap: TOPPGuideAPIContextMap;
    property map: TOPPGuideAPIContextMap read GetMap;

  protected
    constructor Create(AParentContext: IOPPGuideAPIContext = nil);

  public
    class function shared: TOPPGuideAPIContext; static;

    destructor Destroy; override;

    function GetParentStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
    function GetCustomStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;

    { IOPPGuideAPIContext }

    procedure PushStepState(const AResult: TOPPGuideAPIExecutionState);
    function PullStepState(const AStepIdentifier: String): TOPPGuideAPIExecutionState;
    procedure Execute(contextItem: TOPPGuideAPIContextStep; AStepIdentifier: String);
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);
  end;

implementation

uses
  OPP.Guide.API.Executor.RunStateHelper,
  System.SysUtils,
  OPP.Help.Log,
  Vcl.Forms;

function TOPPGuideAPIContext.GetMap: TOPPGuideAPIContextMap;
begin
  if not Assigned(fMap) then
  begin
    fMap := TOPPGuideAPIContextMap.Create;
  end;
  result := fMap;
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

class function TOPPGuideAPIContext.shared: TOPPGuideAPIContext;
begin
  if not Assigned(fContext) then
    fContext := TOPPGuideAPIContext.Create(nil);
  result := fContext;
end;

destructor TOPPGuideAPIContext.Destroy;
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

function TOPPGuideAPIContext.GetParentStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
var
  fPreviousStepState: TOPPGuideAPIExecutionState;
  fParent: TOPPGuideAPIContextStep;
begin

  result := nil;
  fParent := TOPPGuideAPIContextStep(fDataprovider.GetParentStepByIdentifier(AStepIdentifier));
  if not Assigned(fParent) then
    exit;

  fPreviousStepState := PullStepState(fParent.IdentifierFieldValue);
  result := TOPPGuideAPIContextStepResult.Create(fPreviousStepState);
end;

function TOPPGuideAPIContext.GetCustomStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
var
  fPreviousStepState: TOPPGuideAPIExecutionState;
  fParent: TOPPGuideAPIContextStep;
begin

  result := nil;
  fParent := TOPPGuideAPIContextStep(fDataprovider.GetStepByIdentifier(AStepIdentifier));
  if not Assigned(fParent) then
    exit;

  fPreviousStepState := PullStepState(fParent.IdentifierFieldValue);
  result := TOPPGuideAPIContextStepResult.Create(fPreviousStepState);
end;

procedure TOPPGuideAPIContext.PushStepState(const AResult: TOPPGuideAPIExecutionState);
begin
  fMap.AddOrSetValue(AResult.stepIdentifier, AResult);
end;

function TOPPGuideAPIContext.PullStepState(const AStepIdentifier: String): TOPPGuideAPIExecutionState;
begin
  fMap.TryGetValue(AStepIdentifier, result);
end;

procedure TOPPGuideAPIContext.Execute(contextItem: TOPPGuideAPIContextStep; AStepIdentifier: String);
begin
  eventLogger.Flow(Format('Will execute context for Step [%s]', [AStepIdentifier]), 'APIContext');

  contextItem.Execute(AStepIdentifier,
    procedure(AResult: TOPPGuideAPIExecutionState)
    begin
      self.PushStepState(AResult);
      Application.ProcessMessages;
      eventLogger.Flow(Format('Did finish context execution for %s', [AResult.Description]), 'APIContext');
    end);
end;

{ TOPPGuideAPIContext }

procedure TOPPGuideAPIContext.Add(AChild: IOPPGuideAPIContext);
begin

end;

procedure TOPPGuideAPIContext.Remove(AChild: IOPPGuideAPIContext);
begin
  //
end;

procedure TOPPGuideAPIContext.Clear;
begin
  //
end;

procedure TOPPGuideAPIContext.SetDataprovider(AValue: IOPPGuideAPIDataprovider);
begin
  fDataprovider := AValue;
end;

end.
