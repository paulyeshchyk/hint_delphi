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

constructor TOPPHelpShortcutRequest.Create(activeControl: TWinControl; msg: TWMHelp);
begin
  inherited create;
  fMsg := msg;
  fActiveControl := activeControl;
end;

function TOPPHelpShortcutRequest.GetShortcutIdentifier(): String;
begin
  result := fActiveControl.HelpKeyword;
end;

end.
