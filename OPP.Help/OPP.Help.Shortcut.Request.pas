unit OPP.Help.Shortcut.Request;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls;

type
  TOPPHelpShortcutRequest = class
  private
    fMsg: TWMHelp;
    fActiveControl: TWinControl;

    function GetShortcutIdentifier(): String;
  public
    constructor Create(activeControl: TWinControl; msg: TWMHelp);

    property msg: TWMHelp read fMsg;
    property activeControl: TWinControl read fActiveControl;
    property shortcutIdentifier: String read GetShortcutIdentifier;
  end;

implementation

uses OPP.Help.System.Str, OPP.Help.Log;

constructor TOPPHelpShortcutRequest.Create(activeControl: TWinControl; msg: TWMHelp);
var
  activecontrolclassname: String;
begin
  inherited create;
  fMsg := msg;
  fActiveControl := activeControl;

  eventLogger.Log(activeControl.ClassName);
end;

function TOPPHelpShortcutRequest.GetShortcutIdentifier(): String;
  function GetWinControlHelpKeyword(AControl: TWinControl): String;
  begin
    if not Assigned(AControl) then begin
      result := '';
      exit;
    end;

    eventLogger.Log(AControl.ClassName);


    if Length(AControl.HelpKeyword) <> 0 then begin
      result := AControl.HelpKeyword;
      exit;
    end;

    result := GetWinControlHelpKeyword(AControl.Parent);

  end;

begin
  result := GetWinControlHelpKeyword(fActiveControl);
end;



end.
