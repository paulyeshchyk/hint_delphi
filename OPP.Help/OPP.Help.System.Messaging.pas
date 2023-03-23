unit OPP.Help.System.Messaging;

interface

uses
  System.Types, System.Classes,
  WinAPI.Windows, WinAPI.Messages,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.forms;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

  TOPPMessagingHelper = class
  public
    class function GetHWndByPID(const hPID: THandle): THandle;
    class function GetProcessHandle(AProcessName: String): THandle;
  end;

  TOPPMessagePipeCompletion = reference to procedure(AStream: TStream);

  TOPPMessagePipe = class
    function SendRecord(AReceiverHandle: THandle; const ARecordType: ShortString; completion: TOPPMessagePipeCompletion): Boolean;
    function SendStreamData(AReceiverHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): Boolean;
    function SendData(AReceiverHandle: THandle; const ADataToSend: TCopyDataStruct): Boolean;
  end;

implementation

uses TLHelp32, System.Sysutils, OPP.Help.System.Stream;

class function TOPPMessagingHelper.GetHWndByPID(const hPID: THandle): THandle;
type
  PEnumInfo = ^TEnumInfo;

  TEnumInfo = record
    ProcessID: DWORD;
    HWND: THandle;
  end;

  function EnumWindowsProc(Wnd: DWORD; var EI: TEnumInfo): Bool; stdcall;
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

function TOPPMessagePipe.SendRecord(AReceiverHandle: THandle; const ARecordType: ShortString; completion: TOPPMessagePipeCompletion): Boolean;
var
  _Stream: TMemoryStream;
begin
  _Stream := TMemoryStream.Create;
  try
    if Assigned(completion) then
      completion(_Stream);
    _Stream.Position := 0;
    Result := SendStreamData(AReceiverHandle, _Stream, TCopyDataType.cdtRecord);
  finally
    FreeAndNil(_Stream);
  end;
end;

function TOPPMessagePipe.SendStreamData(AReceiverHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): Boolean;
var
  _CopyDataStruct: TCopyDataStruct;
begin
  Result := false;

  if AStream.Size = 0 then
    Exit;

  _CopyDataStruct.dwData := Integer(ADataType);
  _CopyDataStruct.cbData := AStream.Size;
  _CopyDataStruct.lpData := AStream.Memory;

  Result := SendData(AReceiverHandle, _CopyDataStruct);

end;

function TOPPMessagePipe.SendData(AReceiverHandle: THandle; const ADataToSend: TCopyDataStruct): Boolean;
var
  fSendResponse: Integer;
begin
  Result := false;

  if (AReceiverHandle = 0) then
    Exit;

  fSendResponse := SendMessage(AReceiverHandle, WM_COPYDATA, WPARAM(0), LPARAM(@ADataToSend)); // WPARAM(0) -> FLocalReceiverForm.Handle

  Result := fSendResponse <> 0;
end;

end.
