unit OPPClient.Help.Shortcut.Setup;

interface

uses
  system.classes,
  WinAPI.Messages,
  Vcl.Controls, Vcl.Dialogs;

type
  TOPPClientHelpShortcutHelper = class
  public
    class procedure showHelp(AMessage: TWMHelp);
  end;

implementation

uses
  Vcl.Forms,
  {OppObjControl,}
  cxTreeView,

  OPP.Help.Interfaces,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Server,

  OPP.Help.Hint.Server,
  OPP.Help.Hint.Mapping,
  OPP.Help.Hint.Reader,

  OPP.Help.Log;

class procedure TOPPClientHelpShortcutHelper.showHelp(AMessage: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(Screen.ActiveControl, AMessage);
  helpShortcutServer.showHelp(fShortcutRequest, vmExternal,
    procedure(completionResult: TOPPHelpShortcutPresentingResult)
    begin
      if completionResult = prFail then
        ShowMessage('Nothing to show');
    end);
end;

function GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not Assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  eventLogger.Log(AControl.ClassName);

  if AControl.ClassType.InheritsFrom(TcxTreeView) then
  begin
    result := AControl.Name;
  end
  else if AControl.ClassType.InheritsFrom(TForm) then
  begin
    result := AControl.Name;
  end
  else if Length(AControl.HelpKeyword) <> 0 then
  begin
    result := AControl.HelpKeyword;
  end else begin
    result := GetWinControlHelpKeyword(AControl.Parent);
  end;
end;

function ControlHelpIdentifier(AControl: TControl): String;
begin
  result := GetWinControlHelpKeyword(AControl);
end;

function CreateHintReader(AMap: TOPPHelpHintMap): IOPPHelpHintDataReader;
begin
  result := TOPPHelpRichtextHintReader.Create;
  result.loadData(AMap.Predicate.filename);
end;

initialization

helpShortcutServer.setDefaultOnGetIdentifier(ControlHelpIdentifier);
helpHintServer.setDefaultOnHintReaderCreator(CreateHintReader);

finalization

helpHintServer.setDefaultOnHintReaderCreator(nil);
helpShortcutServer.setDefaultOnGetIdentifier(nil);

end.
