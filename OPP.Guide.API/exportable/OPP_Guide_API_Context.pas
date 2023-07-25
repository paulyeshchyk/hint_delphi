unit OPP_Guide_API_Context;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Dataprovider;

type

  IOPPGuideAPIContextListener = interface
    ['{6A59E13D-6BCF-4833-B86C-1EB58C65584B}']
    procedure PushNewExecutionState(AState: TOPPGuideAPIExecutionState);
  end;

  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']

    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure PushStepState(const AResult: TOPPGuideAPIExecutionState);
    function PullStepState(const AStepIdentifier: String): TOPPGuideAPIExecutionState;
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);

    procedure AddListener(AListener: IOPPGuideAPIContextListener);
    procedure RemoveListener(AListener: IOPPGuideAPIContextListener);
  end;

implementation

initialization

finalization

end.
