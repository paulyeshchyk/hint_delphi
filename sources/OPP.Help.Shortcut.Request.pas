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
  public
    property msg: TWMHelp read fMsg;
    property activeControl: TWinControl read fActiveControl;
    constructor create(activeControl: TWinControl; msg: TWMHelp);
  end;

implementation

constructor TOPPHelpShortcutRequest.create(activeControl: TWinControl; msg: TWMHelp);
begin
  inherited create;
  fMsg := msg;
  fActiveControl := activeControl;
end;

end.

