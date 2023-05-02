unit OPP.Help.System.Hook.Keyboard;

interface

uses
  System.SysUtils,
  System.Classes,
  WinAPI.Windows, WinAPI.Messages,
  Vcl.ComCtrls, Vcl.Menus;

type
  PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;

  TKBDLLHOOKSTRUCT = record
    vkCode: cardinal;
    scanCode: cardinal;
    flags: cardinal;
    time: cardinal;
    dwExtraInfo: cardinal;
  end;

  PKeyboardLowLevelHookStruct = ^TKeyboardLowLevelHookStruct;
  TKeyboardLowLevelHookStruct = TKBDLLHOOKSTRUCT;

  TShortcutHelper = record helper for TShortcut
    class function GetShortcutFromHook(AHookStruct: PKBDLLHOOKSTRUCT): TShortcut; static;
    class function GetKeyboardShiftState(AHookStruct: PKBDLLHOOKSTRUCT): TShiftState; static;
  end;

implementation

uses
  Vcl.forms,
  OPP.Keyboard.Shortcut.Manager,
  OPP.Help.System.Messaging;

var
  hhk: HHOOK;
  { TShortcutHelper }

class function TShortcutHelper.GetKeyboardShiftState(AHookStruct: PKBDLLHOOKSTRUCT): TShiftState;

  function isAltDown: Boolean;
  const
    LLKHF_ALTDOWN = $20;
  begin
    result := (LongBool(AHookStruct^.flags and LLKHF_ALTDOWN));
  end;
  function isCtrlDown: Boolean;
  begin
    result := ((GetAsyncKeystate(VK_LCONTROL) <> 0) or (GetAsyncKeystate(VK_RCONTROL) <> 0));
  end;
  function isShiftDown: Boolean;
  begin
    result := ((GetAsyncKeystate(VK_LSHIFT) <> 0) or (GetAsyncKeystate(VK_RSHIFT) <> 0));
  end;

begin
  result := [];
  if isCtrlDown then
    result := result + [ssCtrl];

  if isShiftDown then
    result := result + [ssShift];

  if isAltDown then
    result := result + [ssAlt];
end;

class function TShortcutHelper.GetShortcutFromHook(AHookStruct: PKBDLLHOOKSTRUCT): TShortcut;
begin
  result := Shortcut(AHookStruct^.vkCode, GetKeyboardShiftState(AHookStruct));
end;

{ CBT_FUNC }

function CBT_FUNC(nCode: Integer; AWParam: wParam; ALParam: lParam): LRESULT; stdcall;
var
  fShortcut: TShortcut;
  fHookStruct: PKBDLLHOOKSTRUCT;
begin
  case nCode of
    HC_ACTION:
      begin
        if AWParam = WM_KEYDOWN then
        begin
          fHookStruct := PKBDLLHOOKSTRUCT(Pointer(ALParam));
          fShortcut := TShortcut.GetShortcutFromHook(fHookStruct);
          keyboardShortcutManager.run(fShortcut);
        end;
      end;
  end;
  result := CallNextHookEx(hhk, nCode, AWParam, ALParam);
end;

{ HOOK }

Procedure InitHook();
begin
  hhk := SetWindowsHookEx(WH_KEYBOARD_LL, @CBT_FUNC, 0, 0);
  if hhk = 0 then
    RaiseLastOSError;
end;

Procedure KillHook();
begin
  if (hhk <> 0) then
    UnhookWindowsHookEx(hhk);
  hhk := 0;
end;

initialization

InitHook();

finalization

KillHook();

end.
