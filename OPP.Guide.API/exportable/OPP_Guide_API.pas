unit OPP_Guide_API;

interface

uses
  DBClient,
  System.Variants;

type

  TOPPGuideAPIContextStepState = (osIdle, osRunning, osError, osUnknown);

  IOPPGuideAPIContextStepResult = interface(IUnknown)
  end;

  ROPPGuideAPIContextStepResult = record
    fState: TOPPGuideAPIContextStepState;
    fDescription: String;
    fValue_str: String;
  end;

  TOPPGuideAPIContextStepResult = class
  private
    fRecord: ROPPGuideAPIContextStepResult;
    function GetDescription: String;
    function GetState: TOPPGuideAPIContextStepState;
    function GetValue_str: String;
    procedure SetDescription(const Value: String);
    procedure SetState(const Value: TOPPGuideAPIContextStepState);
    procedure SetValue_str(const Value: String);
  public
    constructor Create();overload;
    constructor Create(ARecord: ROPPGuideAPIContextStepResult);overload;
    property State: TOPPGuideAPIContextStepState read GetState write SetState;
    property Description: String read GetDescription write SetDescription;
    property Value_str: String read GetValue_str write SetValue_str;
    property theRecord: ROPPGuideAPIContextStepResult read fRecord;
  end;

  IOPPGuideAPIContextStep = interface;

  IOPPGuideAPIDataprovider = interface(IUnknown)
    ['{5849F28F-9DFD-4D55-A54B-085A5CD68048}']

    function GetDataset: TClientDataset;
    function GetStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
    function GetParentStepByIdentifier(AIdentifier: String): IOPPGuideAPIContextStep;
    function AddChild(AParentIdentifier: String): IOPPGuideAPIContextStep;
    function Add(): IOPPGuideAPIContextStep;
    function SubsCount(AIdentifier: String): Integer;
    function ActiveItem: IOPPGuideAPIContextStep;
    function ActiveItemSubscCount: Integer;
  end;

  IOPPGuideAPIContext = interface(IUnknown)
    ['{B319476E-98BB-4F45-B68F-9F701950C6C2}']
    procedure Add(AChild: IOPPGuideAPIContext);
    procedure Remove(AChild: IOPPGuideAPIContext);
    procedure Clear;
    procedure PushContextItem(const stepIdentifier: String; const contextItem: IOPPGuideAPIContextStep);
    procedure SetDataprovider(AValue: IOPPGuideAPIDataprovider);
  end;

  IOPPGuideAPIContextStepListener = interface(IUnknown)
    ['{653FF953-BFBC-480B-9367-4499EE59D575}']
  end;

  IOPPGuideAPIContextStep = interface(IUnknown)
    ['{610F0F2E-4034-4310-9F7C-D0D0FCBF9C29}']
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); // IOPPGuideAPIContext
    procedure SetExecutionResult(const AValue: TOPPGuideAPIContextStepResult);
    function GetExecutionResult: TOPPGuideAPIContextStepResult;
  end;

  IOPPGuideAPIIdentifiable = interface(IUnknown)
    ['{0852EEAF-AB86-4F05-92D3-8DE1BA867417}']
    function PIdentifierName: String;
    function IdentifierName: String;
    function IdentifierValue: String;
  end;

implementation

{ TOPPGuideAPIContextStepResult }

constructor TOPPGuideAPIContextStepResult.Create(ARecord: ROPPGuideAPIContextStepResult);
begin
  fRecord := ARecord;
end;

constructor TOPPGuideAPIContextStepResult.Create;
begin
//
end;

function TOPPGuideAPIContextStepResult.GetDescription: String;
begin
  result := fRecord.fDescription;
end;

function TOPPGuideAPIContextStepResult.GetState: TOPPGuideAPIContextStepState;
begin
  result := fRecord.fState;
end;

function TOPPGuideAPIContextStepResult.GetValue_str: String;
begin
  result := fRecord.fValue_str;
end;

procedure TOPPGuideAPIContextStepResult.SetDescription(const Value: String);
begin
  fRecord.fDescription := Value;
end;

procedure TOPPGuideAPIContextStepResult.SetState(const Value: TOPPGuideAPIContextStepState);
begin
  fRecord.fState := Value;
end;

procedure TOPPGuideAPIContextStepResult.SetValue_str(const Value: String);
begin
  fRecord.fValue_str := Value;
end;

end.
