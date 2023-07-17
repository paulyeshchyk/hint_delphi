unit OPP_Guide_API_Context_Helper;

interface

uses
  OPP_Guide_API_Context,
  Proxy_OPPHelpPredicate;

type
  TOPPGuideAPIContextHelper = class(TOPPGuideAPIContext)
  public
    class procedure test(AStepIdentifier: String; AFilename: String; AValue: String; APreviousStepResult: String);static;
  end;

implementation

uses
  OPP_Guide_API_Context_Step_SendMessage_Help;

{ TOPPGuideAPIContextHelper }

class procedure TOPPGuideAPIContextHelper.test(AStepIdentifier: String; AFilename: String; AValue: String; APreviousStepResult: String);
var
  fPredicate: TProxy_OPPHelpPredicate;
  step: TOPPGuideAPIContextStepSendMessageHelp;
begin
  fPredicate := TProxy_OPPHelpPredicate.Create;
  try
    fPredicate.SetFilename('Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf');
    fPredicate.SetValue('Принять решение');
    fPredicate.SetKeywordType(0); // ktSearch

    step := TOPPGuideAPIContextStepSendMessageHelp.Create;
    try
      step.SetPredicate(fPredicate);
      step.TargetApplicationHandle := APreviousStepResult; // previousStepResult.Value_str;

      TOPPGuideAPIContextHelper.shared.Execute(step, AStepIdentifier);
    finally
      step.Free;
    end;

  finally
    fPredicate := nil;
  end;

end;

end.
