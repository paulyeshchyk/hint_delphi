unit OPP_Guide_API;

interface

uses
  DBClient,
  System.Variants,
  System.Classes,
  System.SysUtils,
  OPP_Guide_Executor_State,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Object_Converter;

type

  TOPPGuideAPIContextStepResultCallback = TProc<TOPPGuideExecutorRunState>;

  IOPPGuideScripter = interface
    ['{795CF965-9560-43D0-B7A2-A5CCDAAABC24}']
    function RunScript(AStrings: TStrings): Variant; overload;
    function RunScript(AScriptText: String): Variant; overload;
    function RunScript(AStream: TMemoryStream; AIdentifiable: IOPPGuideAPIIdentifiable): Variant; overload;
    function CompileScript(AStream: TMemoryStream): Variant;
  end;

  IOPPGuideAPIContextStepResult = interface(IUnknown)
    ['{0E910822-6C7B-4B59-AF14-A8CCBC24AB1E}']
  end;

  TOPPGuideAPIContextStepResult = class
  private
    fRecord: TOPPGuideExecutorRunState;
    function GetDescription: String;
    function GetState: TOPPGuideExecutorRunState;
    function GetValue_str: String;
    procedure SetDescription(const value: String);
    procedure SetState(const value: TOPPGuideExecutorRunState);
    procedure SetValue_str(const value: String);
  public
    constructor Create(); overload;
    constructor Create(ARecord: TOPPGuideExecutorRunState); overload;
    property State: TOPPGuideExecutorRunState read GetState write SetState;
    property Description: String read GetDescription write SetDescription;
    property Value_str: String read GetValue_str write SetValue_str;
    property theRecord: TOPPGuideExecutorRunState read fRecord;
  end;

  IOPPGuideAPIContextStep = interface;

  IOPPGuideAPIDataprovider = interface(IUnknown)
    ['{5849F28F-9DFD-4D55-A54B-085A5CD68048}']
    function GetDataset: TClientDataset;
    function GetStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function GetParentStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function AddChild(const AParentIdentifier: String): IOPPGuideAPIIdentifiable;
    function Add(): IOPPGuideAPIIdentifiable;
    function ActiveItem: IOPPGuideAPIIdentifiable;
    function ActiveItemSubscCount: Integer;
    function GetObjectConverter: IOPPGuideObjectConverter;
  end;

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

{ TOPPGuideAPIContextStepResult }

constructor TOPPGuideAPIContextStepResult.Create(ARecord: TOPPGuideExecutorRunState);
begin
  fRecord := ARecord;
end;

constructor TOPPGuideAPIContextStepResult.Create;
begin
  inherited;
end;

function TOPPGuideAPIContextStepResult.GetDescription: String;
begin
  result := fRecord.executionResult;
end;

function TOPPGuideAPIContextStepResult.GetState: TOPPGuideExecutorRunState;
begin
  result := fRecord;
end;

function TOPPGuideAPIContextStepResult.GetValue_str: String;
begin
  result := fRecord.executionResult;
end;

procedure TOPPGuideAPIContextStepResult.SetDescription(const value: String);
begin
  // fRecord.userInfo := value;
end;

procedure TOPPGuideAPIContextStepResult.SetState(const value: TOPPGuideExecutorRunState);
begin
  fRecord := value;
end;

procedure TOPPGuideAPIContextStepResult.SetValue_str(const value: String);
begin
  fRecord.executionResult := value;
end;

end.
