unit OPP.Help.System.Messaging;

interface

uses
  System.Types, System.Classes, System.Generics.Collections, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.Forms;

const
  WM_OPPHook = WM_USER + 800;
  WM_OPPZoom = WM_USER + 801;
  WM_OPPPredicate = WM_USER + 802;
  WM_OPPZoomFit = WM_USER + 803;
  WM_OPPZoomFitReaction = WM_USER + 804;
  WM_OPPNavBarVisible = WM_USER + 805;
  WM_OPPScrollingType = WM_USER + 806;

type
  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TOPPSystemMessageRunCompletion = reference to procedure(ARunResultType: Exception);

  TOPPSystemMessageHelper = class
  private
  public
    class function GetProcessList(): TDictionary<THandle, String>;
    class function GetHWndByPID(const hPID: THandle): THandle;
    class function GetWindowClassHandleList(AWindowClassName: String): TList<THandle>;
    class function GetProcessHandleList(AProcessName: String): TList<THandle>;
    class procedure RunScript(AScript: PWideChar; AHandle: THandle; ActivationDelay: Cardinal; completion: TOPPSystemMessageRunCompletion); overload;
    class procedure RunProcess(AProcessName: String; AHandle: THandle; ActivationDelay: Cardinal; completion: TOPPSystemMessageRunCompletion); overload;
    class function KillProcess(ExeFileName: string): Integer;
  end;

implementation

uses TLHelp32, OPP.Help.System.Str;

class function TOPPSystemMessageHelper.GetProcessList(): TDictionary<THandle, String>;
var
  FSnapshotHandle: THandle; // Process snapshot handle
  FProcessEntry32: TProcessEntry32; // Structural information of the process entry
  ContinueLoop: Boolean;
  fProcessName: String;
begin
  Result := TDictionary<THandle, String>.Create();
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32); // Get the first process in the system
  while ContinueLoop do
  begin
    fProcessName := Lowercase(FProcessEntry32.szExeFile);
    Result.AddOrSetValue(GetHWndByPID(FProcessEntry32.th32ProcessID), fProcessName);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

class function TOPPSystemMessageHelper.GetHWndByPID(const hPID: THandle): THandle;
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

class function TOPPSystemMessageHelper.GetWindowClassHandleList(AWindowClassName: String): TList<THandle>;
var
  fHandle: THandle;
begin
  Result := TList<THandle>.Create();
  fHandle := FindWindow(AWindowClassName.toWideChar, nil);
  if (fHandle <> 0) then
    Result.Add(fHandle);

end;

class function TOPPSystemMessageHelper.GetProcessHandleList(AProcessName: String): TList<THandle>;
var
  fProcessName: string; // Process name
  FSnapshotHandle: THandle; // Process snapshot handle
  FProcessEntry32: TProcessEntry32; // Structural information of the process entry
  ContinueLoop: Boolean;
  fSearchValue: String;
begin

  Result := TList<THandle>.Create;

  fSearchValue := Lowercase(AProcessName);

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // Create a process snapshot
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32); // Get the first process in the system
  while ContinueLoop do
  begin
    fProcessName := Lowercase(WideCharToString(FProcessEntry32.szExeFile));
    if (fProcessName = fSearchValue) then
    begin
      Result.Add(GetHWndByPID(FProcessEntry32.th32ProcessID));
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;

class procedure TOPPSystemMessageHelper.RunProcess(AProcessName: String; AHandle: THandle; ActivationDelay: Cardinal; completion: TOPPSystemMessageRunCompletion);
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
  fCreateProcessResult: Boolean;
  error: Exception;
begin

  System.FillChar(tmpStartupInfo, Sizeof(tmpStartupInfo), 0);
  with tmpStartupInfo do
  begin
    cb := Sizeof(TStartupInfo);
    wShowWindow := SW_SHOWMINIMIZED;
  end;

  fCreateProcessResult := WinAPI.Windows.CreateProcess(nil, AProcessName.toWideChar, nil, nil, true, CREATE_NEW_PROCESS_GROUP, nil, nil, tmpStartupInfo, tmpProcessInformation);
  if not fCreateProcessResult then
  begin
    if assigned(completion) then
    begin
      error := Exception.Create(Format('Process [%s] was not created', [AProcessName]));
      try
        completion(error);
      finally
        error.Free;
      end;
    end;
    exit;
  end;

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hThread);

  // TODO: implement callback or use postmessage to determine if app was executed in time
  WinAPI.Windows.WaitForSingleObject(tmpProcessInformation.hProcess, ActivationDelay);

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hProcess);

  if assigned(completion) then
    completion(nil);

end;

{
  * cmd.exe /C start mailto:test@test.com?subject=A
  * cmd.exe /C start D:\Compiled\Executable\OPPHelpPreview.exe
  * cmd.exe /C D:\Compiled\Executable\OPPHelpPreview.exe
  * D:\Compiled\Executable\OPPHelpPreview.exe
  * rundll32.exe user32.dll,SendMessage 65535 0 0 "Test"
}

class procedure TOPPSystemMessageHelper.RunScript(AScript: PWideChar; AHandle: THandle; ActivationDelay: Cardinal; completion: TOPPSystemMessageRunCompletion);
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
  fCreateProcessResult: Boolean;
  error: Exception;
begin

  System.FillChar(tmpStartupInfo, Sizeof(tmpStartupInfo), 0);
  with tmpStartupInfo do
  begin
    cb := Sizeof(TStartupInfo);
    wShowWindow := SW_SHOWMINIMIZED;
  end;

  fCreateProcessResult := WinAPI.Windows.CreateProcess(nil, AScript, nil, nil, true, CREATE_NEW_PROCESS_GROUP, nil, nil, tmpStartupInfo, tmpProcessInformation);
  if not fCreateProcessResult then
  begin
    if assigned(completion) then
    begin
      error := Exception.Create('Script was not executed');
      try
        completion(error);
      finally
        error.Free;
      end;
    end;
    exit;
  end;

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hThread);

  // TODO: implement callback or use postmessage to determine if app was executed in time
  WinAPI.Windows.WaitForSingleObject(tmpProcessInformation.hProcess, ActivationDelay);

  WinAPI.Windows.CloseHandle(tmpProcessInformation.hProcess);

  if assigned(completion) then
    completion(nil);

end;

class function TOPPSystemMessageHelper.KillProcess(ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  exeFileNameUp, extractedFilename, szExeFile, szFilenameUp: String;
  cmpResult1, cmpResult2: Boolean;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  exeFileNameUp := UpperCase(ExeFileName);
  while Integer(ContinueLoop) <> 0 do
  begin
    szExeFile := FProcessEntry32.szExeFile;
    extractedFilename := UpperCase(ExtractFileName(szExeFile));
    szFilenameUp := UpperCase(szExeFile);
    cmpResult1 := (CompareStr(extractedFilename, exeFileNameUp) = 0);
    cmpResult2 := (CompareStr(szFilenameUp, exeFileNameUp) = 0);
    if (cmpResult1 or cmpResult2) then
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

end.
