unit OPP_Guide_API_Context_Step_SendMessage_Help;

interface

uses
  System.SysUtils,
  OPP_Help_API,
  OPP_Guide_API,
  OPP_Guide_Executor_State,
  OPP_Guide_API_Context_Step,
  Proxy_OPPHelpPredicate;

type
  TOPPGuideAPIContextStepSendMessageHelp = class(TOPPGuideAPIContextStep)
  private
    fHelpApplicationHandle: String;
    [weak]
    fPredicate: TProxy_OPPHelpPredicate;
  public
    procedure SetPredicate(APredicate: TProxy_OPPHelpPredicate);
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback); override;
    property TargetApplicationHandle: String read fHelpApplicationHandle write fHelpApplicationHandle;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,

  OPP_Guide_Executor,

  OPP.Help.Log,
  OPP.Help.Predicate,
  OPP.Help.System.Types,

  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

const
  kContext: String = 'StepSendMessage';

type

  TOPPApplicationHandleValidatorCallback = reference to procedure(AStepIdentifier: String; AHandle: Integer; callback: TOPPGuideAPIContextStepResultCallback);

  TOPPGuideAPIContextStepSendMessageHelpHelper = class helper for TOPPGuideAPIContextStepSendMessageHelp
  public
    procedure ValidateHandleOrThrow(AStepIdentifier: String; AFlowBlock: TOPPApplicationHandleValidatorCallback; ACompletion: TOPPGuideAPIContextStepResultCallback);
    procedure OnFlowExecute(AStepIdentifier: String; AHandle: Integer; callback: TOPPGuideAPIContextStepResultCallback);
    function SendOpenPage(AProcessHandle: THandle; APredicate: IOPPHelpPredicate): TOPPMessagePipeSendResult;
    procedure OnValidHandleCompletion(AStepIdentifier: String; AHandle: Integer; exitBlock: TOPPGuideAPIContextStepResultCallback);
  end;

  { TOPPGuideAPIContextStepOpenHelp }

procedure TOPPGuideAPIContextStepSendMessageHelpHelper.OnValidHandleCompletion(AStepIdentifier: String; AHandle: Integer; exitBlock: TOPPGuideAPIContextStepResultCallback);
var
  fResult: TOPPMessagePipeSendResult;
begin
  fResult := SendOpenPage(AHandle, fPredicate);
  case fResult of
    psrSuccess:
      exitBlock(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, Format('Code:%d', [Integer(fResult)])));
  else
    exitBlock(TOPPGuideExecutorRunState.FinishState(AStepIdentifier, Format('Result:%d', [Integer(fResult)]), ''));
  end;
end;

function TOPPGuideAPIContextStepSendMessageHelpHelper.SendOpenPage(AProcessHandle: THandle; APredicate: IOPPHelpPredicate): TOPPMessagePipeSendResult;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
  fResult: TOPPMessagePipeSendResult;
  fPredicate: TOPPHelpPredicate;
begin
  result := psrFail;
  if AProcessHandle = 0 then
  begin
    exit;
  end;

  if not Assigned(APredicate) then
    exit;

  fSelfHandle := Application.Handle;

  fMessagePipe := TOPPMessagePipe.Create;
  try
    result := fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
      procedure(AStream: TStream)
      begin
        APredicate.WriteToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;

procedure TOPPGuideAPIContextStepSendMessageHelpHelper.OnFlowExecute(AStepIdentifier: String; AHandle: Integer; callback: TOPPGuideAPIContextStepResultCallback);
var
  fResult: TOPPMessagePipeSendResult;
begin

  fResult := SendOpenPage(AHandle, fPredicate);

  case fResult of
    psrSuccess:
      callback(TOPPGuideExecutorRunState.ErrorState(AStepIdentifier, Format('Code:%d', [Integer(fResult)])));
  else
    callback(TOPPGuideExecutorRunState.FinishState(AStepIdentifier, Format('Result:%d', [Integer(fResult)]), ''));
  end;
end;

procedure TOPPGuideAPIContextStepSendMessageHelp.Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
begin

  try

    ValidateHandleOrThrow(AStepIdentifier, OnFlowExecute, callback);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPGuideAPIContextStepSendMessageHelp.SetPredicate(APredicate: TProxy_OPPHelpPredicate);
begin
  fPredicate := APredicate;
end;

procedure TOPPGuideAPIContextStepSendMessageHelpHelper.ValidateHandleOrThrow(AStepIdentifier: String; AFlowBlock: TOPPApplicationHandleValidatorCallback; ACompletion: TOPPGuideAPIContextStepResultCallback);
var
  fHandle: Integer;
begin
  if Length(self.TargetApplicationHandle) = 0 then
    raise Exception.Create('Handle is not defined');

  fHandle := StrToInt(self.TargetApplicationHandle);
  if fHandle = 0 then
    raise Exception.Create('Handle is equal to zero');
  if not Assigned(AFlowBlock) then
    raise Exception.Create('Flow is not defined');
  if not Assigned(ACompletion) then
    raise Exception.Create('Completion is not defined');

  AFlowBlock(AStepIdentifier, fHandle, ACompletion);
end;

end.
