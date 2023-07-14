unit OPP_Guide_API_Context_Step_SendMessage_Help;

interface

uses
  OPP_Guide_API,
  OPP_Guide_API_Context_Step;

type
  TOPPGuideAPIContextStepSendMessageHelp = class(TOPPGuideAPIContextStep)
  private
    fHelpApplicationHandle: String;
    fSearchText: String;
    fHelpFilename: String;
  public
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); override;
    property HelpApplicationHandle: String read fHelpApplicationHandle write fHelpApplicationHandle;
    property HelpFilename: String read fHelpFilename write fHelpFilename;
    property SearchText: String read fSearchText write fSearchText;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,
  OPP.Help.Predicate,
  OPP.Help.System.Types,

  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

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
  fResult : TOPPMessagePipeSendResult;
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

procedure TOPPGuideAPIContextStepSendMessageHelp.PerformIn(AContext: Variant; AStepIdentifier: String);
var
  Predicate: TOPPHelpPredicate;
begin

  Predicate := TOPPHelpPredicate.Create(self.fHelpFilename, TOPPKeywordType.ktSearch, self.fSearchText);
  try

    try
      SendOpenPage(StrToInt(self.HelpApplicationHandle), Predicate);
    except

    end;
  finally
    Predicate.Free;
  end;
end;

end.
