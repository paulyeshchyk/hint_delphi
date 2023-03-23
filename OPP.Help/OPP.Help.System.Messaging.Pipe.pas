unit OPP.Help.System.Messaging.Pipe;

interface

uses
  System.Classes, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

  TOPPMessagePipeCompletion = reference to procedure(AStream: TStream);

  TOPPMessagePipe = class
    function SendRecord(AReceiverHandle: THandle; const ARecordType: ShortString; completion: TOPPMessagePipeCompletion): Boolean;
    function SendStreamData(AReceiverHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): Boolean;
    function SendData(AReceiverHandle: THandle; const ADataToSend: TCopyDataStruct): Boolean;
  end;


implementation


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

