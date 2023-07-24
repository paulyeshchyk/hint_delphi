unit OPP_Guide_API_Context_Step;

interface

uses
  System.SysUtils,
  System.Classes,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Context_Step_Result;

type

  TOPPGuideAPIContextStep = class(TInterfacedObject, IOPPGuideAPIContextStep, IOPPGuideAPIIdentifiable)
  private
    fExecutionResult: TOPPGuideAPIContextStepResult;

    [weak]
    fListener: IOPPGuideAPIContextStepListener;
    fStateDescription: String;
    fIdentifier: String;
    fCaption: String;
    fNodeType: String;
    fPIdentifier: String;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback); virtual;

    function IdentifierName: String;
    function IdentifierValue: String;
    function PIdentifierName: String;
    function PIdentifierValue: String;

    property Listener: IOPPGuideAPIContextStepListener read fListener write fListener;
    // ----------------
    property NodeType: String read fNodeType write fNodeType;
    property Caption: String read fCaption write fCaption;
    property Identifier: String read fIdentifier write fIdentifier;
    property PIdentifier: String read fPIdentifier write fPIdentifier;

  end;

implementation

uses
  OPP.Help.System.Messaging,
  System.Generics.Collections;

{ TOPPTestObject }

constructor TOPPGuideAPIContextStep.Create;
begin
  fExecutionResult := nil;
end;

destructor TOPPGuideAPIContextStep.Destroy;
begin
  if Assigned(fExecutionResult) then
    fExecutionResult.Free;
  inherited;
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

function TOPPGuideAPIContextStep.PIdentifierValue: String;
begin
  result := self.PIdentifier;
end;

procedure TOPPGuideAPIContextStep.Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
begin
end;

end.
