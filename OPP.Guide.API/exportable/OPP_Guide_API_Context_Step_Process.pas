unit OPP_Guide_API_Context_Step_Process;

interface

uses
  Forms,
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API,
  OPP_Guide_API_Context,
  OPP_Guide_API_Context_Step;

type
  TOPPGuideAPIContextStepProcess = class(TOPPGuideAPIContextStep)
  private
    fApplicationName: String;
    fWindowClassName: String;
    procedure SetApplicationName(const Value: String);
  public
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); override;
    property ApplicationName: String read fApplicationName write SetApplicationName;
    property WindowClassName: String read fWindowClassName write fWindowClassName;
  end;

implementation

uses
  OPP.Help.System.Messaging,
  System.Generics.Collections;

{ TOPPGuideAPIContextStepProcess }

procedure TOPPGuideAPIContextStepProcess.PerformIn(AContext: Variant; AStepIdentifier: String);
var
  fContext: TOPPGuideAPIContext;
begin

  self.SetCustomExecutionResult(osError, '', 'not executed');

  fContext := TOPPGuideAPIContext.shared;

  self.SetCustomExecutionResult(osRunning, '');
  TOPPSystemMessageHelper.RunProcess(ApplicationName, Application.Handle, 100,
    procedure(AHandle: System.THandle; ARunResultType: Exception)
    var
      hwnd: THandle;
      fWindowClassHandleList: TList<THandle>;
    begin

      if Assigned(ARunResultType) then
      begin
        self.SetCustomExecutionResult(osError, '', ARunResultType.Message);
        fContext.PushContextItem(AStepIdentifier, self);
        exit;
      end;

      if AHandle = THandle(INVALID_HANDLE_VALUE) then
      begin
        self.SetCustomExecutionResult(osError, '', 'Invalid handle');
        fContext.PushContextItem(AStepIdentifier, self);
        exit;
      end;

      fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fWindowClassName); // 'TOPPHelpPreviewForm'
      if (not Assigned(fWindowClassHandleList)) then
      begin
        self.SetCustomExecutionResult(osError, '', Format('Window Class not found:[%s]', [fWindowClassName]));
        fContext.PushContextItem(AStepIdentifier, self);
        exit;
      end;
      if fWindowClassHandleList.Count = 0 then
      begin
        self.SetCustomExecutionResult(osError, '', Format('Window List is empty for class:[%s]', [fWindowClassName]));
        fContext.PushContextItem(AStepIdentifier, self);
        exit;
      end;

      hwnd := fWindowClassHandleList[0];
      // for hwnd in fWindowClassHandleList do
      // begin
      // end;
      self.SetCustomExecutionResult(osIdle, IntToStr(hwnd));
      fContext.PushContextItem(AStepIdentifier, self);
    end);
end;

procedure TOPPGuideAPIContextStepProcess.SetApplicationName(const Value: String);
begin
  fApplicationName := Value;
end;

end.
