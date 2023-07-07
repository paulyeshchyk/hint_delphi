unit OPP_Guide_API;

interface

uses System.Variants;

type
  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure SetResultForStep(AResult: Variant; AStep: Variant);
    function GetResultForStep(AStep: Variant): Variant;
  end;

  IOPPGuideAPIContextStepListener = interface(IUnknown)
    ['{653FF953-BFBC-480B-9367-4499EE59D575}']
    procedure testDC;
  end;


  IOPPGuideAPIContextStep = interface
    ['{610F0F2E-4034-4310-9F7C-D0D0FCBF9C29}']
    procedure Run(AContext: OLEVariant); // IOPPGuideAPIContext
  end;

implementation

end.
