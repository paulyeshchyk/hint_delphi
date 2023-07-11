unit OPP_Guide_API_Context;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Context_Map;

type

  TOPPGuideAPIContext = class(TInterfacedObject, IOPPGuideAPIContext, IOPPGuideAPIContextStepListener)
  private
    fList: TOPPGuideAPIContextContainer;

    [weak]
    fParentContext: IOPPGuideAPIContext;
    fMap: TOPPGuideAPIContextMap;

    function GetMap: TOPPGuideAPIContextMap;
    property map: TOPPGuideAPIContextMap read GetMap;
    class var fContext: TOPPGuideAPIContext;

  protected
    constructor Create(AParentContext: IOPPGuideAPIContext = nil);

  public

    class function shared: TOPPGuideAPIContext; static;

    destructor Destroy; override;

    { IOPPGuideAPIContext }
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure SetResultForStep(AResult: Variant; AStep: Variant);
    function GetResultForStep(AStep: Variant): Variant;

    procedure testDC;
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

constructor TOPPGuideAPIContext.Create(AParentContext: IOPPGuideAPIContext);
begin
  inherited Create;
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

function TOPPGuideAPIContext.GetResultForStep(AStep: Variant): Variant;
begin
  self.map.TryGetValue(AStep, result);
end;

procedure TOPPGuideAPIContext.Remove(AChild: IOPPGuideAPIContext);
begin

end;

procedure TOPPGuideAPIContext.SetResultForStep(AResult, AStep: Variant);
begin
  self.map.AddOrSetValue(AStep, AResult);
end;

class function TOPPGuideAPIContext.shared: TOPPGuideAPIContext;
begin
  if not Assigned(fContext) then
  begin
    fContext := TOPPGuideAPIContext.Create(nil);
  end;
  result := fContext;

end;

procedure TOPPGuideAPIContext.testDC;
begin
  //
end;

initialization

finalization

end.
