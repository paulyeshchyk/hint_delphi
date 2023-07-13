unit OPP_Guide_API_Context_Step_OpenHelp;

interface

uses
  OPP.Help.Predicate,
  OPP_Guide_API,
  OPP_Guide_API_Context_Step;

type
  TOPPGuideAPIContextStepOpenHelp = class(TOPPGuideAPIContextStep)
  private
    fPredicate: TOPPHelpPredicate;
    procedure findhelpandstartsearch(APredicate: TOPPHelpPredicate);
    function SendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): Integer;
  public
    procedure SetPredicate(APredicate: TOPPHelpPredicate);
    procedure PerformIn(AContext: Variant; AStepIdentifier: String); override;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.System.Messaging,
  OPP.Help.System.AppExecutor;

{ TOPPGuideAPIContextStepOpenHelp }

function TOPPGuideAPIContextStepOpenHelp.SendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): Integer;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
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
    { result := } fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
      procedure(AStream: TStream)
      begin
        // Predicate.WriteToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;

procedure TOPPGuideAPIContextStepOpenHelp.findhelpandstartsearch(APredicate: TOPPHelpPredicate);
begin
  TOPPHelpSystemAppExecutor.Execute('OPPHelpPreview.exe', // kPreviewFormProcessName
    procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType)
    var
      hwnd: THandle;
      sentResult: TOPPMessagePipeSendResult;
      Error: Exception;
    begin
      if not(Assigned(AList) and (AList.Count <> 0)) then
      begin
        // if Assigned(completion) then
        // begin
        // Error := Exception.Create('Help app was not executed');
        // try
        // completion(Error);
        // finally
        // Error.Free;
        // end;
        // end;
        exit;
      end;

      for hwnd in AList do
      begin
        // ForceForegroundWindow(hwnd);
        { sentResult := } SendOpenPage(hwnd, APredicate);
        // eventLogger.Flow(Format(SEventSentMessageResultTemplate, [sentResult.asString]), kContext);
      end;

      // if Assigned(completion) then
      // completion(nil);
    end);
end;

procedure TOPPGuideAPIContextStepOpenHelp.PerformIn(AContext: Variant; AStepIdentifier: String);
begin

  try
    self.findhelpandstartsearch(fPredicate);
  except

  end;
end;

procedure TOPPGuideAPIContextStepOpenHelp.SetPredicate(APredicate: TOPPHelpPredicate);
begin
  fPredicate := APredicate;
end;

end.
