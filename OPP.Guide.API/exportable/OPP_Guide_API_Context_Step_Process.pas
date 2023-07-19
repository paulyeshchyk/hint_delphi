unit OPP_Guide_API_Context_Step_Process;

interface

uses
  Forms,
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API,
  OPP_Guide_API_Context,
  OPP_Guide_API_Context_Step, OPP_Guide_API_Context_Step_SendMessage_Help;

type
  TOPPGuideAPIContextStepProcess = class(TOPPGuideAPIContextStep)
  private
    fApplicationName: String;
    fWindowClassName: String;
    fWaitForSingleObjectInMS: Cardinal;
  public
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback); override;
    property ApplicationName: String read fApplicationName write fApplicationName;
    property WindowClassName: String read fWindowClassName write fWindowClassName;
    property WaitForSingleObjectInMS: Cardinal read fWaitForSingleObjectInMS write fWaitForSingleObjectInMS default 1000;
  end;

implementation

uses
  OPP_Guide_Executor,
  OPP.Help.System.Messaging,
  System.Generics.Collections,
  OPP.Help.Log, OPP_Guide_Executor_State;

const
  kContext: String = 'StepProcess';

{ TOPPGuideAPIContextStepProcess }

procedure TOPPGuideAPIContextStepProcess.Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
var
  fContext: TOPPGuideAPIContext;
begin

  fContext := TOPPGuideAPIContext.shared;

  TOPPSystemMessageHelper.RunProcess(ApplicationName, Application.Handle, WaitForSingleObjectInMS,
    procedure(AHandle: System.THandle; ARunResultType: Exception)
    var
      hwnd: THandle;
      fWindowClassHandleList: TList<THandle>;
    begin

      if not Assigned(callback) then begin
        eventLogger.Error('callback is not defined',kContext);
        exit;
      end;

      if Assigned(ARunResultType) then
      begin
        callback(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, ARunResultType.Message));
        exit;
      end;

      if AHandle = THandle(INVALID_HANDLE_VALUE) then
      begin
        callback(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, 'Invalid handle'));
        exit;
      end;

      fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fWindowClassName);
      if (not Assigned(fWindowClassHandleList)) then
      begin
        callback(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, Format('Window Class not found:[%s]', [fWindowClassName])));
        exit;
      end;
      if fWindowClassHandleList.Count = 0 then
      begin
        callback(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, Format('Window List is empty for class:[%s]', [fWindowClassName])));
        exit;
      end;

      hwnd := fWindowClassHandleList[0];
      // for hwnd in fWindowClassHandleList do
      // begin
      // end;
//      self.SetCustomExecutionResult(osIdle, IntToStr(hwnd));
//      fContext.PushContextItem(AStepIdentifier, self);
      callback(TOPPGuideExecutorRunState.FinishState(AStepIdentifier, '', IntToStr(hwnd)));

    end);
end;

end.
