unit OPP_Guide_API_Context_Step;

interface

uses
  Forms,
  Variants,
  System.SysUtils,
  System.Classes,
  OPP_Guide_API,
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.Predicate;

type
  TOPPTestObjectState = (osIdle, osRunning, osError);

  TOPPGuideAPIContextStep = class(TInterfacedObject, IOPPGuideAPIContextStep)
  private
    fState: TOPPTestObjectState;
    fStepResult: Variant;

    [weak]
    fListener: IOPPGuideAPIContextStepListener;
    fStateDescription: String;
    procedure SetState(const Value: TOPPTestObjectState);
  public
    procedure Run(AContext: OLEVariant); virtual;
    property State: TOPPTestObjectState read fState write SetState default osIdle;
    property StateDescription: String read fStateDescription write fStateDescription;
    property StepResult: Variant read fStepResult;
    property Listener: IOPPGuideAPIContextStepListener read fListener write fListener;
  end;

  TOPPGuideAPIContextStepProcess = class(TOPPGuideAPIContextStep)
  private
    fApplicationName: String;
  public
    procedure Run(AContext: OLEVariant); override;
    property ApplicationName: String read fApplicationName write fApplicationName;
  end;

  TOPPGuideAPIContextStepOpenHelp = class(TOPPGuideAPIContextStep)
  private
    fFilename: String;
    fSearchTextValue: String;
  private
    function sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
    procedure findhelpandstartsearch(APredicate: TOPPHelpPredicate);
  public
    procedure Run(AContext: OLEVariant); override;
    property Filename: String read fFilename write fFilename;
    property SearchTextValue: String read fSearchTextValue write fSearchTextValue;
  end;

implementation

uses
  System.Generics.Collections,
  OPP.Help.System.Types,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

{ TOPPTestObject }

procedure TOPPGuideAPIContextStep.Run(AContext: OLEVariant);
begin
end;

{ TOPPGuideAPIContextStepProcess }

procedure TOPPGuideAPIContextStepProcess.Run(AContext: OLEVariant);
begin

  fStepResult := Null;

  if (VarIsNull(AContext) or VarIsEmpty(AContext)) then
    exit;

  self.State := osRunning;
  TOPPSystemMessageHelper.RunProcess(ApplicationName, Application.Handle, 100,
    procedure(AHandle: System.THandle; ARunResultType: Exception)
    var fThread: TThread;
    begin
      fThread := TThread.currentThread;
      TThread.Synchronize(nil,
        procedure
        begin

          if Assigned(ARunResultType) then begin
            fState := osError;
            fStateDescription := ARunResultType.Message;
            exit;
          end;

          if AHandle = THandle(INVALID_HANDLE_VALUE) then begin
            fState := osError;
            fStateDescription := 'Invalid handle';
            exit;
          end;

          fStepResult := 0;

          try
            AContext.testDC;
            fState := osIdle;
          except
            on E: Exception do begin
              fState := osError;
              fStateDescription := E.Message;
            end;
          end;
        end);
    end);
end;

procedure TOPPGuideAPIContextStep.SetState(const Value: TOPPTestObjectState);
begin
  fState := Value;
  case fState of
    osIdle: fStateDescription := 'idle';
    osRunning: fStateDescription := 'running';
    osError: fStateDescription := 'error';
  end;
end;

{ TOPPGuideAPIContextStepOpenHelp }

function TOPPGuideAPIContextStepOpenHelp.sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
begin
  result := psrFail;
  if AProcessHandle = 0 then
  begin
    // ShowMessage('Невозможно запустить окно помощи.');
    exit;
  end;

  fSelfHandle := Application.Handle;

  fMessagePipe := TOPPMessagePipe.Create;
  try
    result := fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
      procedure(AStream: TStream)
      begin
        Predicate.WriteToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;


procedure TOPPGuideAPIContextStepOpenHelp.findhelpandstartsearch(APredicate: TOPPHelpPredicate);
begin
  TOPPHelpSystemAppExecutor.Execute('OPPHelpPreview.exe',//kPreviewFormProcessName
    procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType)
    var
      hwnd: THandle;
      sentResult: TOPPMessagePipeSendResult;
      Error: Exception;
    begin
      if not(Assigned(AList) and (AList.Count <> 0)) then
      begin
//        if Assigned(completion) then
//        begin
//          Error := Exception.Create('Help app was not executed');
//          try
//            completion(Error);
//          finally
//            Error.Free;
//          end;
//        end;
        exit;
      end;

      for hwnd in AList do
      begin
        //ForceForegroundWindow(hwnd);
        sentResult := sendOpenPage(hwnd, APredicate);
        //eventLogger.Flow(Format(SEventSentMessageResultTemplate, [sentResult.asString]), kContext);
      end;

//      if Assigned(completion) then
//        completion(nil);
    end);
end;

procedure TOPPGuideAPIContextStepOpenHelp.Run(AContext: OLEVariant);
var
  predicate: TOPPHelpPredicate;
begin
  inherited;
  predicate := TOPPHelpPredicate.Create;
  try
    predicate.filename := self.Filename;
    predicate.keywordType := TOPPKeywordType.ktSearch;
    predicate.value := self.SearchTextValue;
    self.findhelpandstartsearch(predicate);




  finally
    predicate.free;
  end;
end;

end.
