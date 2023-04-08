unit OPP.Help.System.Hook.Keyboard;

interface


implementation

uses
  OPP.Help.System.Constants,
  WinAPI.Windows,
  System.SysUtils,
  vcl.forms;

var
  hhk: HHOOK;

function CBT_FUNC(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall;

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
  isLCtrlDown: boolean;
  isLShiftDown: boolean;
  isF12Down: boolean;
const
  LLKHF_ALTDOWN = $20;
begin
  case nCode of
    HC_ACTION:
      begin
        isLCtrlDown := GetAsyncKeyState(VK_CONTROL) and $8000 <> 0;
        isLShiftDown := GetAsyncKeyState(VK_Shift) and $8000 <> 0;
        isF12Down := GetAsyncKeyState(VK_F12) and $8000 <> 0;
        if (isLCtrlDown and isLShiftDown and isF12Down) then
        begin
          OutputDebugString('Hooked');
          PostMessage(GetForegroundWindow, OPP_KEYBOARD_HOOK_MESSAGE, 0, 0);
        end;
      end;
  end;
  Result := CallNextHookEx(hhk, nCode, wParam, lParam);
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

initialization

InitHook();

finalization

KillHook();

end.
