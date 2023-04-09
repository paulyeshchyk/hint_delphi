unit OPP.Help.System.Messaging.Pipe;

interface

uses
  System.Classes, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

  TOPPMessagePipeSendResult = (psrSuccess = 0, psrFail = 1, psrFailDueNilStream = 2, psrFailDueZeroSizeStream = 3, psrFailDueUnknownReceiver = 4);
  TOPPMessagePipeSentResultHelper = record helper for TOPPMessagePipeSendResult
  public
    function asString(): String;
  end;


  TOPPMessagePipeStreamFullfillBlock = reference to procedure(AStream: TStream);

  TOPPMessagePipe = class
    function SendRecord(AReceiverHandle: THandle; ASenderHandle: THandle; const ARecordType: ShortString; StreamFullfillBlock: TOPPMessagePipeStreamFullfillBlock): TOPPMessagePipeSendResult;
    function SendStreamData(AReceiverHandle: THandle; ASenderHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): TOPPMessagePipeSendResult;
    function SendCopyDataMessage(AReceiverHandle: THandle; ASenderHandle: THandle; const ADataToSend: TCopyDataStruct): TOPPMessagePipeSendResult;
  end;

const
  SMessageResultSuccess = 10000;

implementation

uses
  OPP.Help.Log;

resourcestring
  SEventReceivedMessageResultTemplate = 'Received message result: %d';
  SEventSentMessage = 'Sent message';
  kEventFlowName = 'Pipe';

function TOPPMessagePipe.SendRecord(AReceiverHandle: THandle; ASenderHandle: THandle; const ARecordType: ShortString; StreamFullfillBlock: TOPPMessagePipeStreamFullfillBlock): TOPPMessagePipeSendResult;
var
  fStream: TMemoryStream;
begin
  fStream := TMemoryStream.Create;
  try
    if Assigned(StreamFullfillBlock) then
    begin
      StreamFullfillBlock(fStream);
      fStream.Position := 0;
      Result := SendStreamData(AReceiverHandle, ASenderHandle, fStream, TCopyDataType.cdtRecord);
    end else begin
      Result := psrFailDueNilStream;
    end;
  finally
    FreeAndNil(fStream);
  end;
end;

function TOPPMessagePipe.SendStreamData(AReceiverHandle: THandle; ASenderHandle: THandle; const AStream: TMemoryStream; const ADataType: TCopyDataType): TOPPMessagePipeSendResult;
var
  fCopyDataStruct: TCopyDataStruct;
begin
  Result := psrFailDueZeroSizeStream;

  if AStream.Size = 0 then
    Exit;

  fCopyDataStruct.dwData := Integer(ADataType);
  fCopyDataStruct.cbData := AStream.Size;
  fCopyDataStruct.lpData := AStream.Memory;

  Result := SendCopyDataMessage(AReceiverHandle, ASenderHandle, fCopyDataStruct);
end;

function TOPPMessagePipe.SendCopyDataMessage(AReceiverHandle: THandle; ASenderHandle: THandle; const ADataToSend: TCopyDataStruct): TOPPMessagePipeSendResult;
begin
  Result := psrFailDueUnknownReceiver;

  if (AReceiverHandle = 0) then
    Exit;

  eventLogger.Flow(SEventSentMessage, kEventFlowName);

  // should always sendmessage be used;
  // see https://devblogs.microsoft.com/oldnewthing/20110916-00/?p=9623
  if SendMessage(AReceiverHandle, WM_COPYDATA, Integer(ASenderHandle), LPARAM(@ADataToSend)) = SMessageResultSuccess then
    result := psrSuccess
  else
    result := psrFail;

  eventLogger.Flow(Format(SEventReceivedMessageResultTemplate, [Integer(result)]), kEventFlowName);

end;

{ TOPPMessagePipeSentResultHelper }

function TOPPMessagePipeSentResultHelper.asString: String;
begin
  case self of
    psrSuccess: result := 'Success';
    psrFail: result := 'Fail';
    psrFailDueNilStream: result := 'Stream is not defined';
    psrFailDueZeroSizeStream: result := 'Stream size is zero';
    psrFailDueUnknownReceiver: result := 'Unknown receiver';
  end;
end;

end.
