unit OPP_Guide_API_Context_Step;

interface

uses
  // ComObj,
  Forms,
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API;

type
  {
    procedure TOPPGuideAPIContextStep.SetState(const Value: TOPPGuideAPIContextStepState);
    begin
    fState := Value;
    case fState of
    osIdle:
    fStateDescription := 'idle';
    osRunning:
    fStateDescription := 'running';
    osError:
    fStateDescription := 'error';
    end;
    end;

  }

  TOPPGuideAPIContextStep = class(TInterfacedObject, IOPPGuideAPIContextStep, IOPPGuideAPIIdentifiable)
  private
    fExecutionResult: TOPPGuideAPIContextStepResult;

    [weak]
    fListener: IOPPGuideAPIContextStepListener;
    fStateDescription: String;
    fActionIdentifier: String;
    fReactionIdentifier: String;
    fIdentifier: String;
    fCaption: String;
    fNodeType: String;

  protected
    procedure SetCustomExecutionResult(AState: TOPPGuideAPIContextStepState; AValue: String; ADescription: String = '');

  public
    constructor Create;

    function GetTest: TOPPGuideAPIContextStepResult;
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); virtual;

    procedure SetExecutionResult(const AValue: TOPPGuideAPIContextStepResult);
    function GetExecutionResult: TOPPGuideAPIContextStepResult;
    function IdentifierName: String;
    function IdentifierValue: String;
    function PIdentifierName: String;

    property ExecutionResult: TOPPGuideAPIContextStepResult read GetExecutionResult write SetExecutionResult;

    property StateDescription: String read fStateDescription write fStateDescription;
    property Listener: IOPPGuideAPIContextStepListener read fListener write fListener;
    // ----------------
    property NodeType: String read fNodeType write fNodeType;
    property Caption: String read fCaption write fCaption;
    property ReactionIdentifier: String read fReactionIdentifier write fReactionIdentifier;
    property ActionIdentifier: String read fActionIdentifier write fActionIdentifier;
    property Identifier: String read fIdentifier write fIdentifier;

  end;

implementation

uses
  OPP.Help.System.Messaging,
  System.Generics.Collections;

{ TOPPTestObject }

constructor TOPPGuideAPIContextStep.Create;
begin
end;

function TOPPGuideAPIContextStep.GetExecutionResult: TOPPGuideAPIContextStepResult;
begin
  result := fExecutionResult;
end;

function TOPPGuideAPIContextStep.GetTest: TOPPGuideAPIContextStepResult;
begin
  result.state := osIdle;
end;

function TOPPGuideAPIContextStep.IdentifierName: String;
begin
  result := 'identifier';
end;

function TOPPGuideAPIContextStep.IdentifierValue: String;
begin
  result := self.Identifier;
end;

function TOPPGuideAPIContextStep.PIdentifierName: String;
begin
  result := 'pidentifier';
end;

procedure TOPPGuideAPIContextStep.PerformIn(AContext: Variant; AStepIdentifier: String);
begin
end;

procedure TOPPGuideAPIContextStep.SetCustomExecutionResult(AState: TOPPGuideAPIContextStepState; AValue: String; ADescription: String);
begin
  fExecutionResult.state := AState;
  fExecutionResult.value_str := AValue;
  fExecutionResult.description := ADescription;
end;

procedure TOPPGuideAPIContextStep.SetExecutionResult(const AValue: TOPPGuideAPIContextStepResult);
begin
  fExecutionResult := AValue;
end;

end.
