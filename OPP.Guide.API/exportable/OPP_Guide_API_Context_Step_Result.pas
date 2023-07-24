unit OPP_Guide_API_Context_Step_Result;

interface

uses
  OPP_Guide_API;

type
  TOPPGuideAPIContextStepResult = class
  private
    fRecord: TOPPGuideAPIExecutionState;
    function GetDescription: String;
    function GetState: TOPPGuideAPIExecutionState;
    function GetValue_str: String;
    procedure SetDescription(const value: String);
    procedure SetState(const value: TOPPGuideAPIExecutionState);
    procedure SetValue_str(const value: String);
  public
    constructor Create(); overload;
    constructor Create(ARecord: TOPPGuideAPIExecutionState); overload;
    property State: TOPPGuideAPIExecutionState read GetState write SetState;
    property Description: String read GetDescription write SetDescription;
    property Value_str: String read GetValue_str write SetValue_str;
    property theRecord: TOPPGuideAPIExecutionState read fRecord;
  end;

implementation

{ TOPPGuideAPIContextStepResult }

constructor TOPPGuideAPIContextStepResult.Create(ARecord: TOPPGuideAPIExecutionState);
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

function TOPPGuideAPIContextStepResult.GetState: TOPPGuideAPIExecutionState;
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

procedure TOPPGuideAPIContextStepResult.SetState(const value: TOPPGuideAPIExecutionState);
begin
  fRecord := value;
end;

procedure TOPPGuideAPIContextStepResult.SetValue_str(const value: String);
begin
  fRecord.executionResult := value;
end;

end.
