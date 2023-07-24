unit OPP_Guide_API_Context;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Dataprovider;

type

  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']

    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure PushStepState(const AResult: TOPPGuideAPIExecutionState);
    function PullStepState(const AStepIdentifier: String): TOPPGuideAPIExecutionState;
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);
  end;

implementation

initialization

finalization

end.
