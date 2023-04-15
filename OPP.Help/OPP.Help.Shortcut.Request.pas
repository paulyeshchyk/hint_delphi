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
    fActiveControl: TControl;

  public
    constructor Create(AActiveControl: TControl; msg: TWMHelp);overload;
    constructor Create(AActiveControl: TControl);overload;

    property messag: TWMHelp read fMsg;
    property ActiveControl: TControl read fActiveControl;
  end;

implementation

uses OPP.Help.Log;

constructor TOPPHelpShortcutRequest.Create(AActiveControl: TControl; msg: TWMHelp);
begin
  inherited Create;
  fMsg := msg;
  fActiveControl := AActiveControl;

  eventLogger.Debug(fActiveControl.ClassName);
end;

constructor TOPPHelpShortcutRequest.Create(AActiveControl: TControl);
begin
  inherited Create;
  fActiveControl := AActiveControl;
  eventLogger.Debug(fActiveControl.ClassName);
end;

end.
