unit OPP.Help.System.Messaging;

interface

uses
  System.Types, System.Classes,
  WinAPI.Windows,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.Forms;

type

  TOPPMessagingHelper = class
  public
    class function GetHWndByPID(const hPID: THandle): THandle;
    class function GetProcessHandle(AProcessName: String): THandle;
  end;

implementation

uses TLHelp32, System.Sysutils;

class function TOPPMessagingHelper.GetHWndByPID(const hPID: THandle): THandle;
type
  PEnumInfo = ^TEnumInfo;

  TEnumInfo = record
    ProcessID: DWORD;
    HWND: THandle;
  end;

  function EnumWindowsProc(Wnd: DWORD; var EI: TEnumInfo): Boolean; stdcall;
  var
    PID: DWORD;
  begin
    GetWindowThreadProcessID(Wnd, @PID);
    Result := (PID <> EI.ProcessID) or (not IsWindowVisible(Wnd)) or (not IsWindowEnabled(Wnd));

    if not Result then
      EI.HWND := Wnd;
  end;

  function FindMainWindow(PID: DWORD): DWORD;
  var
    fEnumInfo: TEnumInfo;
  begin
    fEnumInfo.ProcessID := PID;
    fEnumInfo.HWND := 0;
    EnumWindows(@EnumWindowsProc, Integer(@fEnumInfo));
    Result := fEnumInfo.HWND;
  end;

begin
  if hPID <> 0 then
    Result := FindMainWindow(hPID)
  else
    Result := 0;
end;

class function TOPPMessagingHelper.GetProcessHandle(AProcessName: String): THandle;
var
  fProcessName: string; // Process name
  FSnapshotHandle: THandle; // Process snapshot handle
  FProcessEntry32: TProcessEntry32; // Structural information of the process entry
  ContinueLoop: Boolean;
begin

  Result := 0;

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32); // Get the first process in the system
  while ContinueLoop do
  begin
    fProcessName := Lowercase(FProcessEntry32.szExeFile);

    if (fProcessName = AProcessName) then
    begin
      Result := GetHWndByPID(FProcessEntry32.th32ProcessID);
      ContinueLoop := false;
    end else begin
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  end;

  CloseHandle(FSnapshotHandle);
end;

end.
