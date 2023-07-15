unit OPP_Guide_API_Context_Step_SendMessage_Help;

interface

uses
  System.SysUtils,
  OPP_Guide_API,
  OPP_Guide_Executor_State,
  OPP_Guide_API_Context_Step;

type
  TOPPGuideAPIContextStepSendMessageHelp = class(TOPPGuideAPIContextStep)
  private
    fHelpApplicationHandle: String;
    fSearchText: String;
    fHelpFilename: String;
  public
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback); override;
    property HelpApplicationHandle: String read fHelpApplicationHandle write fHelpApplicationHandle;
    property HelpFilename: String read fHelpFilename write fHelpFilename;
    property SearchText: String read fSearchText write fSearchText;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,
  OPP.Help.Log,
  OPP.Help.Predicate,
  OPP.Help.System.Types,

  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

const
  kContext: String = 'StepSendMessage';

type
  TOPPGuideAPIContextStepSendMessageHelpHelper = class helper for TOPPGuideAPIContextStepSendMessageHelp
  public
    function SendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): Integer;
  end;

  { TOPPGuideAPIContextStepOpenHelp }

function TOPPGuideAPIContextStepSendMessageHelpHelper.SendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): Integer;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
  fResult: TOPPMessagePipeSendResult;
begin
  result := 10001; // psrFail;
  if AProcessHandle = 0 then
  begin
    // ShowMessage('Невозможно запустить окно помощи.');
    exit;
  end;

  fSelfHandle := Application.Handle;

  fMessagePipe := TOPPMessagePipe.Create;
  try
    fResult := fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
      procedure(AStream: TStream)
      begin
        Predicate.WriteToStream(AStream);
      end);
    result := Integer(fResult);
  finally
    fMessagePipe.Free;
  end;
end;

procedure TOPPGuideAPIContextStepSendMessageHelp.Execute(AStepIdentifier: String; callback: TOPPGuideAPIContextStepResultCallback);
var
  fPredicate: TOPPHelpPredicate;
  fHandle: Integer;
begin

  fHandle := 0;
  if Length(Self.HelpApplicationHandle) = 0 then
  begin
    eventLogger.Error('Handle is not defined', kContext);
    exit;
  end;
  try
    fHandle := StrToInt(Self.HelpApplicationHandle);
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;

  if fHandle = 0 then
  begin
    eventLogger.Error('Handle is equal to zero', kContext);
    exit;
  end;

  fPredicate := TOPPHelpPredicate.Create(Self.fHelpFilename, TOPPKeywordType.ktSearch, Self.fSearchText);
  try

    try
      SendOpenPage(StrToInt(Self.HelpApplicationHandle), fPredicate);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    fPredicate.Free;
  end;
end;

end.
