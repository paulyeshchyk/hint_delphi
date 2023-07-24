unit OPP_Guide_API_Context_Step_Process;

interface

uses
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API,
  OPP_Guide_API_Context,
  OPP_Guide_API_Context_Step,
  OPP_Guide_API_Context_Step_SendMessage_Help;

type
  TOPPGuideAPIContextStepProcess = class(TOPPGuideAPIContextStep)
  private
    fApplicationName: String;
    fWindowClassName: String;
    fWaitForSingleObjectInMS: Cardinal;
    procedure OnExecutionComplete(AStepIdentifier: String; AHandle: System.THandle;ffWindowClassName: String; ARunResultType: Exception; ACallback: TOPPGuideAPIExecutionStateCallback);
  public
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIExecutionStateCallback); override;
    property ApplicationName: String read fApplicationName write fApplicationName;
    property WindowClassName: String read fWindowClassName write fWindowClassName;
    property WaitForSingleObjectInMS: Cardinal read fWaitForSingleObjectInMS write fWaitForSingleObjectInMS default 1000;
  end;

implementation

uses
  OPP.Help.System.Messaging,
  OPP.Guide.API.Executor.RunStateHelper,
  System.Generics.Collections,
  OPPGuideAPIContext,
  OPP.Help.Log,
  Forms,
  WinAPI.Windows,
  System.Threading;

const
  kContext: String = 'StepProcess';

  { TOPPGuideAPIContextStepProcess }

procedure TOPPGuideAPIContextStepProcess.Execute(AStepIdentifier: String; callback: TOPPGuideAPIExecutionStateCallback);
var
  fContext: TOPPGuideAPIContext;
  fAppHandle: THandle;
  fAppName: String;
  ffWaitForSingleObjectInMS: Cardinal;
  ffWindowClassName: String;
begin
  fContext := TOPPGuideAPIContext.shared;
  fAppHandle := Application.Handle;
  fAppName := self.ApplicationName;
  ffWaitForSingleObjectInMS := self.WaitForSingleObjectInMS;
  ffWindowClassName := self.WindowClassName;

  TTask.Run(
    procedure()
    begin
      TOPPSystemMessageHelper.RunProcess(fAppName, fAppHandle, ffWaitForSingleObjectInMS,
        procedure(AHandle: System.THandle; ARunResultType: Exception)
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              self.OnExecutionComplete(AStepIdentifier, AHandle, ffWindowClassName, ARunResultType, callback);
            end);
        end);
    end);
end;

procedure TOPPGuideAPIContextStepProcess.OnExecutionComplete(AStepIdentifier: String; AHandle: System.THandle;ffWindowClassName: String; ARunResultType: Exception; ACallback: TOPPGuideAPIExecutionStateCallback);
var
  hwnd: THandle;
  fWindowClassHandleList: TList<THandle>;

begin

  if not Assigned(ACallback) then
  begin
    eventLogger.Error('callback is not defined', kContext);
    exit;
  end;

  if Assigned(ARunResultType) then
  begin
    ACallback(TOPPGuideAPIExecutionState.Error(AStepIdentifier, ARunResultType.Message));
    exit;
  end;

  if AHandle = THandle(INVALID_HANDLE_VALUE) then
  begin
    ACallback(TOPPGuideAPIExecutionState.Error(AStepIdentifier, 'Invalid handle'));
    exit;
  end;

  fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(ffWindowClassName);
  if (not Assigned(fWindowClassHandleList)) then
  begin
    ACallback(TOPPGuideAPIExecutionState.Error(AStepIdentifier, Format('Window Class not found:[%s]', [ffWindowClassName])));
    exit;
  end;
  if fWindowClassHandleList.Count = 0 then
  begin
    ACallback(TOPPGuideAPIExecutionState.Error(AStepIdentifier, Format('Window List is empty for class:[%s]', [ffWindowClassName])));
    exit;
  end;

  hwnd := fWindowClassHandleList[0];
  // for hwnd in fWindowClassHandleList do
  // begin
  // end;
  // self.SetCustomExecutionResult(osIdle, IntToStr(hwnd));
  // fContext.PushContextItem(AStepIdentifier, self);
  ACallback(TOPPGuideAPIExecutionState.finished(AStepIdentifier, IntToStr(hwnd)));

end;

end.
