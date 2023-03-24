unit OPP.Help.System.Messaging;

interface

uses
  System.Types, System.Classes, System.Generics.Collections,
  WinAPI.Windows, WinAPI.ShellAPI,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.Forms;

type

  TOPPMessagingHelplerRunProcessCompletion = reference to procedure;

  TOPPMessagingHelper = class
  private
    class function GetProcessList(): TDictionary<THandle, String>;
  public
    class function GetHWndByPID(const hPID: THandle): THandle;
    class function GetWindowClassHandleList(AWindowClassName: String): TList<THandle>;
    class function GetProcessHandleList(AProcessName: String): TList<THandle>;
    class function RunProcess(AProcessName: String; AHandle: THandle; WaitForDelay: Integer; completion: TOPPMessagingHelplerRunProcessCompletion): Boolean;
    class function KillProcess(ExeFileName: string): Integer;
  end;

implementation

uses TLHelp32, System.Sysutils, OPP.Help.System.Str;

class function TOPPMessagingHelper.GetProcessList(): TDictionary<THandle, String>;
var
  FSnapshotHandle: THandle; // Process snapshot handle
  FProcessEntry32: TProcessEntry32; // Structural information of the process entry
  ContinueLoop: Boolean;
  fProcessName: String;
begin
  result := TDictionary<THandle, String>.Create();
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32); // Get the first process in the system
  while ContinueLoop do
  begin
    fProcessName := Lowercase(FProcessEntry32.szExeFile);
    result.AddOrSetValue(GetHWndByPID(FProcessEntry32.th32ProcessID), fProcessName);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

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
    result := (PID <> EI.ProcessID) or (not IsWindowVisible(Wnd)) or (not IsWindowEnabled(Wnd));

    if not result then
      EI.HWND := Wnd;
  end;

  function FindMainWindow(PID: DWORD): DWORD;
  var
    fEnumInfo: TEnumInfo;
  begin
    fEnumInfo.ProcessID := PID;
    fEnumInfo.HWND := 0;
    EnumWindows(@EnumWindowsProc, Integer(@fEnumInfo));
    result := fEnumInfo.HWND;
  end;

begin
  if hPID <> 0 then
    result := FindMainWindow(hPID)
  else
    result := 0;
end;

class function TOPPMessagingHelper.GetWindowClassHandleList(AWindowClassName: String): TList<THandle>;
var
  fHandle: THandle;
begin
  result := TList<THandle>.Create();
  fHandle := FindWindow(AWindowClassName.toWideChar, nil);
  if (fHandle <> 0) then
    result.Add(fHandle);

end;

class function TOPPMessagingHelper.GetProcessHandleList(AProcessName: String): TList<THandle>;
var
  fProcessName: string; // Process name
  FSnapshotHandle: THandle; // Process snapshot handle
  FProcessEntry32: TProcessEntry32; // Structural information of the process entry
  ContinueLoop: Boolean;
  fSearchValue: String;
  ffHandle: THandle;
begin

  result := TList<THandle>.Create;

  fSearchValue := Lowercase(AProcessName);

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32); // Get the first process in the system
  while ContinueLoop do
  begin
    fProcessName := Lowercase(WideCharToString(FProcessEntry32.szExeFile));
    if (fProcessName = fSearchValue) then
    begin
      result.Add(GetHWndByPID(FProcessEntry32.th32ProcessID));
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;

{


  function  FileExec( const CmdLine: String; bHide, bWait: Boolean): Boolean;
  var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  s:String;
  begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
  cb := SizeOf(TStartupInfo);
  dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  if bHide then
  wShowWindow := SW_HIDE
  else
  wShowWindow := SW_SHOWNORMAL;
  end;

  s:=ExtractFilePath(cmdLine);

  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, False,
  NORMAL_PRIORITY_CLASS, nil,pChar(s), StartupInfo, ProcessInfo);
  if Result then
  CloseHandle(ProcessInfo.hThread);

  if bWait then
  if Result then
  begin
  WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  end;
  if Result then
  CloseHandle(ProcessInfo.hProcess);
  end;
}

class function TOPPMessagingHelper.RunProcess(AProcessName: String; AHandle: THandle; WaitForDelay: Integer;  completion: TOPPMessagingHelplerRunProcessCompletion): Boolean;
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
begin

  result := false;

  System.FillChar(tmpStartupInfo, Sizeof(tmpStartupInfo), 0);
  with tmpStartupInfo do
  begin
    cb := Sizeof(TStartupInfo);
    wShowWindow := SW_SHOWMINIMIZED;
  end;

  result := WinAPI.Windows.CreateProcess(nil, AProcessName.toWideChar, nil, nil, true, CREATE_NEW_PROCESS_GROUP, nil, nil, tmpStartupInfo, tmpProcessInformation);
  if not result then
    exit;

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hThread);

  WinAPI.Windows.WaitForSingleObject(tmpProcessInformation.hProcess, WaitForDelay);

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hProcess);
//  WinAPI.Windows.CloseHandle(tmpProcessInformation.dwThreadId);

  if Assigned(completion) then
    completion();

end;

class function TOPPMessagingHelper.KillProcess(ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))) then
      result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

end.
