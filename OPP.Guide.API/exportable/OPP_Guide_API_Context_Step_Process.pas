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
    procedure SetApplicationName(const Value: String);
  public
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); override;
    property ApplicationName: String read fApplicationName write SetApplicationName;
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
      fThread: TThread;
    begin
      fThread := TThread.currentThread;
      TThread.Synchronize(nil,
        procedure
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

          self.SetCustomExecutionResult(osIdle, IntToStr(AHandle));
          fContext.PushContextItem(AStepIdentifier, self);
        end);
    end);
end;

procedure TOPPGuideAPIContextStepProcess.SetApplicationName(const Value: String);
begin
  fApplicationName := Value;
end;

end.
