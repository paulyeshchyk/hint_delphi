unit OPPClient.Help.Shortcut.Setup;

interface

uses
  system.classes,
  WinAPI.Messages,
  Vcl.Forms,
  Vcl.Controls, Vcl.Dialogs;

type
  TOPPClientHelpShortcutHelper = class
  public
    class procedure showHelp(AControl: TControl; AMessage: TWMHelp);
  end;

implementation

uses
  OppObjControl,
  OppAttrControl,
  cxTreeView,

  OPP.Help.Interfaces,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Server,

  OPP.Help.Hint.Server,
  OPP.Help.Map,
  OPP.Help.Hint.Reader,

  OPP.Help.Log,
  OPPClientChild;

class procedure TOPPClientHelpShortcutHelper.showHelp(AControl: TControl; AMessage: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(AControl, AMessage);
  try
    helpShortcutServer.showHelp(fShortcutRequest, vmExternal,
      procedure(completionResult: TOPPHelpShortcutPresentingResult)
      begin
        if completionResult = prFail then
          eventLogger.Warning('not able to open help form');
      end);
  finally
    fShortcutRequest.Free;
  end;
end;

function GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not Assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  if AControl.ClassType.InheritsFrom(TOppObjControl) then
  begin
    result := TOppObjControl(AControl).TypeObject;
  end
  else if AControl.ClassType.InheritsFrom(TOppAttrControl) then
  begin
    result := TOppAttrControl(AControl).Attribute;
  end
  else if AControl.ClassType.InheritsFrom(TcxTreeView) then
  begin
    result := AControl.Name;
  end
  else if AControl.ClassType.InheritsFrom(TfmChild) then
  begin
    result := TfmChild(AControl).Task.Name;
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

function CreateHintReader(AMap: TOPPHelpMap): IOPPHelpHintDataReader;
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
