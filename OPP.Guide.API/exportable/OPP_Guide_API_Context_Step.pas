unit OPP_Guide_API_Context_Step;

interface

uses
  Forms,
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API;

type
  TOPPTestObjectState = (osIdle, osRunning, osError);

  TOPPGuideAPIContextStep = class(TInterfacedObject, IOPPGuideAPIContextStep)
  private
    fState: TOPPTestObjectState;
    fStepResult: Variant;

    [weak]
    fListener: IOPPGuideAPIContextStepListener;
    fStateDescription: String;
    procedure SetState(const Value: TOPPTestObjectState);
  public
    procedure Run(AContext: OLEVariant); virtual;
    property State: TOPPTestObjectState read fState write SetState default osIdle;
    property StateDescription: String read fStateDescription write fStateDescription;
    property StepResult: Variant read fStepResult;
    property Listener: IOPPGuideAPIContextStepListener read fListener write fListener;
  end;

  TOPPGuideAPIContextStepProcess = class(TOPPGuideAPIContextStep)
  private
    fApplicationName: String;
  public
    procedure Run(AContext: OLEVariant); override;
    property ApplicationName: String read fApplicationName write fApplicationName;
  end;

implementation

uses
  OPP.Help.System.Messaging;

{ TOPPTestObject }

procedure TOPPGuideAPIContextStep.Run(AContext: OLEVariant);
begin
end;

{ TOPPGuideAPIContextStepProcess }

procedure TOPPGuideAPIContextStepProcess.Run(AContext: OLEVariant);
begin
  if (VarIsNull(AContext) or VarIsEmpty(AContext)) then
    exit;

  self.State := osRunning;
  TOPPSystemMessageHelper.RunProcess(ApplicationName, Application.Handle, 100,
    procedure(ARunResultType: Exception)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          fStepResult := Null;

          try
            AContext.testDC;
            fState := osIdle;
          except
            on E: Exception do begin
              fState := osError;
              fStateDescription := E.Message;
            end;
          end;
        end);
    end);
end;

procedure TOPPGuideAPIContextStep.SetState(const Value: TOPPTestObjectState);
begin
  fState := Value;
  case fState of
    osIdle: fStateDescription := 'idle';
    osRunning: fStateDescription := 'running';
    osError: fStateDescription := 'error';
  end;
end;

end.
