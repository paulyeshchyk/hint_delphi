unit OPP_Guide_API_Context_Step_SendMessage_Help;

interface

uses
  System.SysUtils,
  OPP_Help_API,
  OPP_Guide_API,
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
    property Predicate: TProxy_OPPHelpPredicate read fPredicate write SetPredicate;
    property TargetApplicationHandle: String read fHelpApplicationHandle write fHelpApplicationHandle;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,

  OPP.Help.Log,
  OPP.Help.Predicate,
  OPP.Help.System.Types,
  OPP.Guide.API.Executor.RunStateHelper,
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

const
  kContext: String = 'StepSendMessage';

type

  TOPPApplicationHandleValidatorCallback = reference to procedure(AStepIdentifier: String; AHandle: Integer; callback: TOPPGuideAPIContextStepResultCallback);

  TOPPGuideAPIContextStepSendMessageHelpHelper = class helper for TOPPGuideAPIContextStepSendMessageHelp
  public
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
      exitBlock(TOPPGuideExecutorRunState.error(AStepIdentifier, Format('Code:%d', [Integer(fResult)])));
  else
    exitBlock(TOPPGuideExecutorRunState.finished(AStepIdentifier, Format('Result:%d', [Integer(fResult)])));
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
  if not Assigned(callback) then
    raise Exception.Create('Completion is not defined');

  fResult := SendOpenPage(AHandle, fPredicate);

  case fResult of
    psrSuccess:
      callback(TOPPGuideExecutorRunState.finished(AStepIdentifier, Format('Result:%d', [Integer(fResult)])));
  else
    callback(TOPPGuideExecutorRunState.error(AStepIdentifier, Format('Code:%d', [Integer(fResult)])));
  end;
end;

procedure TOPPGuideAPIContextStepSendMessageHelp.Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
var
  fHandle: Integer;
begin
  if Length(self.TargetApplicationHandle) = 0 then
    raise Exception.Create('Handle is not defined');
  fHandle := StrToInt(self.TargetApplicationHandle);
  if fHandle = 0 then
    raise Exception.Create('Handle is equal to zero');

  OnFlowExecute(AStepIdentifier, fHandle, callback);
end;

procedure TOPPGuideAPIContextStepSendMessageHelp.SetPredicate(APredicate: TProxy_OPPHelpPredicate);
begin
  fPredicate := APredicate;
end;

end.
