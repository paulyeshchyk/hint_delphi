unit OPP_Guide_API_Context;

interface

uses
  System.SysUtils,
  OPP_Guide_API,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Context_Map,
  OPP_Guide_Executor_State;

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
    procedure onStepExecution(AResult: TOPPGuideExecutorRunState);

  protected
    constructor Create(AParentContext: IOPPGuideAPIContext = nil);

  public
    class function shared: TOPPGuideAPIContext; static;

    destructor Destroy; override;

    procedure PushStepState(const AResult: TOPPGuideExecutorRunState);
    function PullStepState(const AStepIdentifier: String): TOPPGuideExecutorRunState;

    { IOPPGuideAPIContext }

    procedure Execute(const contextItem: TOPPGuideAPIContextStep; AStepIdentifier: String);

    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;

    function GetParentStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
    function GetCustomStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;

    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);
  end;

implementation

uses
  Vcl.Forms,
  Windows,
  System.Variants,
  OPP.Help.Log,
  OPP_Guide_Executor,
  OPP_Guide_Executor_State_Helper;

const
  kContext: String = 'APIContext';

  { TOPPGuideAPIContext }

procedure TOPPGuideAPIContext.Add(AChild: IOPPGuideAPIContext);
begin

end;

procedure TOPPGuideAPIContext.Clear;
begin
  //
end;

function TOPPGuideAPIContext.PullStepState(const AStepIdentifier: String): TOPPGuideExecutorRunState;
begin
  fMap.TryGetValue(AStepIdentifier, result);
end;

procedure TOPPGuideAPIContext.PushStepState(const AResult: TOPPGuideExecutorRunState);
begin
  fMap.AddOrSetValue(AResult.stepIdentifier, AResult);
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

procedure TOPPGuideAPIContext.Execute(const contextItem: TOPPGuideAPIContextStep; AStepIdentifier: String);
begin
  eventLogger.Flow(Format('Started step [%s]', [AStepIdentifier]), kContext);
  contextItem.Execute(AStepIdentifier, onStepExecution);
end;

function TOPPGuideAPIContext.GetCustomStepResult(AStepIdentifier: String): TOPPGuideAPIContextStepResult;
var
  fPreviousStepState: TOPPGuideExecutorRunState;
  fParent: TOPPGuideAPIContextStep;
begin

  result := nil;
  fParent := TOPPGuideAPIContextStep(fDataprovider.GetStepByIdentifier(AStepIdentifier));
  if not Assigned(fParent) then
    exit;

  fPreviousStepState := PullStepState(fParent.IdentifierValue);
  result := TOPPGuideAPIContextStepResult.Create(fPreviousStepState);
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
  fPreviousStepState: TOPPGuideExecutorRunState;
  fParent: TOPPGuideAPIContextStep;
begin

  result := nil;
  fParent := TOPPGuideAPIContextStep(fDataprovider.GetParentStepByIdentifier(AStepIdentifier));
  if not Assigned(fParent) then
    exit;

  fPreviousStepState := PullStepState(fParent.IdentifierValue);
  result := TOPPGuideAPIContextStepResult.Create(fPreviousStepState);
end;

procedure TOPPGuideAPIContext.onStepExecution(AResult: TOPPGuideExecutorRunState);
begin
  self.PushStepState(AResult);

  Application.ProcessMessages;
  case AResult.value of
    rsvError:
      eventLogger.Error(AResult.Description, kContext);
  else
    eventLogger.Flow(AResult.Description, kContext);
  end;
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
