unit OPP.Help.System.Timer;

interface

uses System.Classes, System.SysUtils, WinAPI.Windows;

type
  TOPPThreadTimerExecuteEvent = reference to procedure();

  TOPPThreadTimer = class(TThread)
  private
    FTickEvent: THandle;
    fInterval: Cardinal;
    fEvent: TOPPThreadTimerExecuteEvent;
    fStarted: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; executionEvent: TOPPThreadTimerExecuteEvent; interval: Cardinal = 200);
    destructor Destroy; override;
    procedure FinishThreadExecution;
  end;

implementation

{ TOPPThreadTimer }

constructor TOPPThreadTimer.Create(CreateSuspended: Boolean; executionEvent: TOPPThreadTimerExecuteEvent; interval: Cardinal);
begin
  inherited Create(CreateSuspended);
  fStarted := false;
  FreeOnTerminate := True;
  FTickEvent := CreateEvent(nil, false, false, nil);
  fEvent := executionEvent;
  fInterval := interval;
end;

destructor TOPPThreadTimer.Destroy;
begin
  SetEvent(FTickEvent);
  CloseHandle(FTickEvent);
  inherited;
end;

procedure TOPPThreadTimer.Execute;
begin
  fStarted := True;
  while not Terminated do
  begin
    try
      // if WaitForSingleObject(Self.Handle, fInterval) = WAIT_TIMEOUT then
      if WaitForSingleObject(FTickEvent, fInterval) = WAIT_TIMEOUT then
      begin
        Synchronize(
          procedure
          begin
            if Assigned(fEvent) then
              fEvent;
          end);
      end;
    except
      on E: Exception do
      begin
        OutputDebugString('');
      end;
    end;
  end;
end;

procedure TOPPThreadTimer.FinishThreadExecution;
begin
  if fStarted then
  begin
    Terminate;
    SetEvent(FTickEvent);
  end;
  fStarted := false;
end;

end.
