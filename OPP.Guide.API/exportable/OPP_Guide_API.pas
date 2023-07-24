unit OPP_Guide_API;

interface

uses
  System.Variants,
  System.Classes,
  System.SysUtils,
  OPP_Guide_API_Object_Converter,
  OPP_Guide_API_Dataprovider,
  OPP_Guide_API_Identifiable;

type
  TOPPGuideExecutorRunStateValue = (rsvIdle = 0, rsvStarted = 1, rsvProgress = 2, rsvFinished = 3, rsvError = -1);

  TOPPGuideExecutorRunState = record
    stepIdentifier: String;
    value: TOPPGuideExecutorRunStateValue;
    executionResult: String;
  end;

  TOPPGuideAPIContextStepResultCallback = TProc<TOPPGuideExecutorRunState>;

  IOPPGuideAPIContextStepResult = interface(IUnknown)
    ['{0E910822-6C7B-4B59-AF14-A8CCBC24AB1E}']
  end;

  IOPPGuideAPIContextStep = interface;

  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure PushStepState(const AResult: TOPPGuideExecutorRunState);
    function PullStepState(const AStepIdentifier: String): TOPPGuideExecutorRunState;
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);
  end;

  IOPPGuideAPIContextStepListener = interface(IUnknown)
    ['{653FF953-BFBC-480B-9367-4499EE59D575}']
  end;

  IOPPGuideAPIContextStep = interface(IUnknown)
    ['{610F0F2E-4034-4310-9F7C-D0D0FCBF9C29}']
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
  end;

implementation

end.
