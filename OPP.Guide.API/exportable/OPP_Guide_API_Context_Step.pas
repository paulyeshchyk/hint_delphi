unit OPP_Guide_API_Context_Step;

interface

uses
  System.SysUtils,
  OPP_Guide_API,
  OPP_Guide_API_Context,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Context_Step_Result;

type
  TOPPGuideAPIContextStep = class(TOPPGuideAPIScriptContainer, IOPPGuideAPIExecutable)
  private
    fExecutionResult: TOPPGuideAPIContextStepResult;

    fStateDescription: String;
    fIdentifier: String;
    fCaption: String;
    fNodeType: String;
    fPIdentifier: String;

  public

    constructor Create;
    destructor Destroy; override;

    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIExecutionStateCallback); virtual;

    // ----------------
    property NodeType: String read fNodeType write fNodeType;
    property Caption: String read fCaption write fCaption;

  end;

implementation

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

procedure TOPPGuideAPIContextStep.Execute(AStepIdentifier: String; callback: TOPPGuideAPIExecutionStateCallback);
begin
end;

end.
