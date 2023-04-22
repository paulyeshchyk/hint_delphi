unit OPP.Help.System.Hook.Keyboard;

interface

uses
  System.SysUtils,
  System.Classes,
  WinAPI.Windows, WinAPI.Messages,
  Vcl.ComCtrls, Vcl.Menus;

type

  TOPPHotKey = record
    vkKey: Byte;
    shift: Boolean;
    alt: Boolean;
    ctrl: Boolean;
  end;

  TOPPHotKeyHelper = record helper for TOPPHotKey
    class function GetHotKey(AHotKey: THotKey): TOPPHotKey; static;
  end;

  TShortcutHelper = record helper for TShortcut
    class function GetShortcutFromKeyboard(): TShortcut; static;
  end;

implementation

uses
  Vcl.forms,
  OPP.Keyboard.Shortcut.Manager,
  OPP.Help.System.Messaging;

var
  hhk: HHOOK;

function CBT_FUNC(nCode: Integer; AWParam: wParam; ALParam: lParam): LRESULT; stdcall;

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

var
  isLCtrlDown: Boolean;
  isLShiftDown: Boolean;
  isLAltDown: Boolean;
  fShortcut: TShortcut;
  fShiftState: TShiftState;
  p: PKBDLLHOOKSTRUCT;
const
  LLKHF_ALTDOWN = $20;

begin
  case nCode of
    HC_ACTION:
      begin

        p := PKBDLLHOOKSTRUCT(Pointer(ALParam));
        isLCtrlDown := GetAsyncKeystate(VK_CONTROL) and $8000 <> 0;
        isLShiftDown := GetAsyncKeystate(VK_SHIFT) and $8000 <> 0;
        isLAltDown := LongBool(p^.flags and LLKHF_ALTDOWN);

        fShiftState := [];
        if isLCtrlDown then
          fShiftState := fShiftState + [ssCtrl];
        if isLShiftDown then
          fShiftState := fShiftState + [ssShift];
        if isLAltDown then
          fShiftState := fShiftState + [ssAlt];
        fShortcut := Shortcut(p^.vkCode, fShiftState);
        if AWParam = WM_KEYDOWN then
          keyboardShortcutManager.run(fShortcut);

      end;
  end;
  result := CallNextHookEx(hhk, nCode, AWParam, ALParam);
end;

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
end;

{ TShortcutHelper }

class function TShortcutHelper.GetShortcutFromKeyboard: TShortcut;
var
  Mods: Byte;
  vkKey, i: word;
begin
  // result := Shortcut(

  // isLCtrlDown := GetAsyncKeystate(VK_CONTROL) and $8000 <> 0;
  // isLShiftDown := GetAsyncKeystate(VK_SHIFT) and $8000 <> 0;
  // isF12Down := GetAsyncKeystate(VK_F12) and $8000 <> 0;
  // if (isLCtrlDown and isLShiftDown and isF12Down) then
  // begin
  // OutputDebugString('Hooked');
  // PostMessage(GetForegroundWindow, WM_OPPHook, 0, 0);
  // end;

  Mods := (GetAsyncKeystate(VK_SHIFT) shr 31 shl 1) or (GetAsyncKeystate(VK_CONTROL) shr 31 shl 2) or (GetAsyncKeystate(VK_MENU) shr 31 shl 3);
  i := 0;
  vkKey := 0;
  while i < 256 do
  begin
    if GetAsyncKeystate(i) <> 0 then
      vkKey := i;
    i := i + 1;
  end;
  result := Mods shl 8 or vkKey;
end;

{ TOPPHotKeyHelper }

class function TOPPHotKeyHelper.GetHotKey(AHotKey: THotKey): TOPPHotKey;
var
  bMod: Byte;
begin
  bMod := AHotKey.HotKey shr 8;
  result.vkKey := AHotKey.HotKey and $00FF;
  result.shift := bMod and 2 > 0;
  result.ctrl := bMod and 4 > 0;
  result.alt := bMod and 8 > 0;
end;

initialization

InitHook();

finalization

KillHook();

end.
