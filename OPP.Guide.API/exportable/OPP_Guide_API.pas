unit OPP_Guide_API;

interface

uses System.Variants;

type

  TOPPGuideAPIContextStepState = (osIdle, osRunning, osError);

  TOPPGuideAPIContextStepResult = record
    state: TOPPGuideAPIContextStepState;
    description: String;
    value_str: String;
  end;

  IOPPGuideAPIContextStep = interface;

  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure PushContextItem(const stepIdentifier: String; const contextItem: IOPPGuideAPIContextStep);

  end;

  IOPPGuideAPIContextStepListener = interface(IUnknown)
    ['{653FF953-BFBC-480B-9367-4499EE59D575}']
    procedure testDC;
  end;

  IOPPGuideAPIContextStep = interface(IUnknown)
    ['{610F0F2E-4034-4310-9F7C-D0D0FCBF9C29}']
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); // IOPPGuideAPIContext
    procedure SetExecutionResult(const AValue: TOPPGuideAPIContextStepResult);
    function GetExecutionResult: TOPPGuideAPIContextStepResult;
  end;

  IOPPGuideAPIIdentifiable = interface (IUnknown)
  ['{0852EEAF-AB86-4F05-92D3-8DE1BA867417}']
    function PIdentifierName: String;
    function IdentifierName: String;
    function IdentifierValue: String;
  end;

implementation


end.
