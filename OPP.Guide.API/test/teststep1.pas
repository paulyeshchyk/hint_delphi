unit teststep1;

interface

implementation

uses
  OPP_Guide_API_Context_Step_Process,
  OPPGuideAPIContext;

function Execute(AStepIdentifier: String): Integer;
var
  step: TOPPGuideAPIContextStepProcess;
  context: TOPPGuideAPIContext;
begin

  context := TOPPGuideAPIContext.Shared;

  result := -1;
  step := TOPPGuideAPIContextStepProcess.Create;
  try
    step.ApplicationName := 'OPPHelpPreview.exe';
    step.WindowClassName := 'TOPPHelpPreviewForm';
    step.WaitForSingleObjectInMS := 1000;
    context.Execute(step, AStepIdentifier);
    result := 0;
  finally
    step.free;
  end;
end;

end.
