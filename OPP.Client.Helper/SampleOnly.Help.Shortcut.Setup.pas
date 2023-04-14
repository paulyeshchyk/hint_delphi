unit SampleOnly.Help.Shortcut.Setup;

interface

uses
  System.Classes,
  WinAPI.Messages,
  Vcl.Controls, Vcl.Dialogs,

  OPP.Help.System.References;

type
  TOPPClientHelpShortcutHelper = class
  public
    class procedure showHelp(AMessage: TWMHelp);
    class function SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
  end;

implementation

uses
  Vcl.Forms, System.SysUtils,
  {OppObjControl,}
  cxTreeView,

  OPP.Help.Interfaces,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Server,

  OPP.Help.Hint.Server,
  OPP.Help.Map,
  OPP.Help.Hint.Reader,
  OPP.Help.System.Error,
  OPP.Help.Log;

class procedure TOPPClientHelpShortcutHelper.showHelp(AMessage: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(Screen.ActiveControl, AMessage);
  try
    helpShortcutServer.showHelp(fShortcutRequest, vmExternal,
      procedure(error: Exception)
      begin
        if error = nil then
          exit;
        eventLogger.Error(error);
      end);
  finally
    fShortcutRequest.Free;
  end;
end;

class function TOPPClientHelpShortcutHelper.SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
begin
  result := helpShortcutServer.SaveMaps(AFileName, callback);
end;

function GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not Assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  eventLogger.Debug(AControl.ClassName);

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
