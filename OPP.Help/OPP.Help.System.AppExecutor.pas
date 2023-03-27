unit OPP.Help.System.AppExecutor;

interface

uses
  System.Generics.Collections,
  WinAPI.Windows, Vcl.Forms,

  OPP.Help.System.Messaging;

type
  TOPPHelpSystemAppExecutionResultType = (rtFailedDueUnableToRunProcess, rtNewInstance, rtExistingInstance);

  TOPPHelpSystemAppExecutorCompletion = reference to procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType);

  TOPPHelpSystemAppExecutor = class
  public
    class procedure Execute(Appname: String; AWindowClass: TClass; completion: TOPPHelpSystemAppExecutorCompletion; AActivationDelay: Cardinal = 300);
  end;

implementation

{ TOPPHelpSystemAppExecutor }

class procedure TOPPHelpSystemAppExecutor.Execute(Appname: String; AWindowClass: TClass; completion: TOPPHelpSystemAppExecutorCompletion; AActivationDelay: Cardinal);
var
  fWindowClassHandleList: TList<THandle>;
  fSelfHandle: THandle;
  fOPPViewerClassName: String;
begin
  fOPPViewerClassName := AWindowClass.classname;

  fSelfHandle := Application.Handle;

  fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
  if Assigned(fWindowClassHandleList) and (fWindowClassHandleList.Count <> 0) then
  begin
    completion(fWindowClassHandleList, rtExistingInstance);
    exit;
  end;

  TOPPSystemMessageHelper.RunProcess(Appname, fSelfHandle, AActivationDelay,
    procedure(ARunResultType: TOPPSystemMessageRunResultType)
    begin
      case ARunResultType of
        rrtFail:
          begin
            completion(nil, rtFailedDueUnableToRunProcess);
          end;
        rrtSuccess:
          begin
            fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
            completion(fWindowClassHandleList, rtNewInstance);
          end;
      end;
    end);

end;

end.
