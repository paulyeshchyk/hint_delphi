unit OPP.Help.System.Messaging.Pipe;

interface

uses
  System.Classes, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

  TOPPMessagePipeSendResult = Integer;
  TOPPMessagePipeCompletion = reference to procedure(AStream: TStream);

  TOPPMessagePipe = class
    function SendRecord(AReceiverHandle: THandle; ASenderHandle: THandle; const ARecordType: ShortString; completion: TOPPMessagePipeCompletion): TOPPMessagePipeSendResult;
    function SendStreamData(AReceiverHandle: THandle; ASenderHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): TOPPMessagePipeSendResult;
    function SendData(AReceiverHandle: THandle; ASenderHandle: THandle; const ADataToSend: TCopyDataStruct): TOPPMessagePipeSendResult;
  end;

implementation

function TOPPMessagePipe.SendRecord(AReceiverHandle: THandle; ASenderHandle: THandle; const ARecordType: ShortString; completion: TOPPMessagePipeCompletion): TOPPMessagePipeSendResult;
var
  fStream: TMemoryStream;
begin
  fStream := TMemoryStream.Create;
  try
    if Assigned(completion) then
      completion(fStream);
    fStream.Position := 0;
    Result := SendStreamData(AReceiverHandle, ASenderHandle, fStream, TCopyDataType.cdtRecord);
  finally
    FreeAndNil(fStream);
  end;
end;

function TOPPMessagePipe.SendStreamData(AReceiverHandle: THandle; ASenderHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): TOPPMessagePipeSendResult;
var
  fCopyDataStruct: TCopyDataStruct;
begin
  Result := -1;

  if AStream.Size = 0 then
    Exit;

  fCopyDataStruct.dwData := Integer(ADataType);
  fCopyDataStruct.cbData := AStream.Size;
  fCopyDataStruct.lpData := AStream.Memory;

  Result := SendData(AReceiverHandle, ASenderHandle, fCopyDataStruct);
end;

function TOPPMessagePipe.SendData(AReceiverHandle: THandle; ASenderHandle: THandle; const ADataToSend: TCopyDataStruct): TOPPMessagePipeSendResult;
begin
  Result := -1;

  if (AReceiverHandle = 0) then
    Exit;

  result := SendMessage(AReceiverHandle, WM_COPYDATA, Integer(ASenderHandle), LPARAM(@ADataToSend));
end;

end.
