unit OPP.Help.System.Messaging.Pipe;

interface

uses
  System.Classes, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

  TOPPMessagePipeSendResult = (psrSuccess = 10000, psrFail = 10001, psrFailDueNilStream = 10002, psrFailDueZeroSizeStream = 10003, psrFailDueUnknownReceiver = 10004, psrFailDueBrokenPredicate = 10005);

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
var
  fResult: NativeInt;
begin
  Result := psrFailDueUnknownReceiver;

  if (AReceiverHandle = 0) then
    Exit;

  eventLogger.Flow(SEventSentMessage, kEventFlowName);

  // should always sendmessage be used;
  // see https://devblogs.microsoft.com/oldnewthing/20110916-00/?p=9623
  fResult := SendMessage(AReceiverHandle, WM_COPYDATA, Integer(ASenderHandle), LPARAM(@ADataToSend));
  case TOPPMessagePipeSendResult(fResult) of
    psrSuccess:
      Result := psrSuccess;
    psrFailDueNilStream:
      Result := psrFail;
    psrFailDueZeroSizeStream:
      Result := psrFail;
    psrFailDueUnknownReceiver:
      Result := psrFail;
  end;

  eventLogger.Flow(Format(SEventReceivedMessageResultTemplate, [Integer(Result)]), kEventFlowName);

end;

{ TOPPMessagePipeSentResultHelper }

function TOPPMessagePipeSentResultHelper.asString: String;
begin
  case self of
    psrSuccess:
      Result := 'Success';
    psrFail:
      Result := 'Fail';
    psrFailDueNilStream:
      Result := 'Stream is not defined';
    psrFailDueZeroSizeStream:
      Result := 'Stream size is zero';
    psrFailDueUnknownReceiver:
      Result := 'Unknown receiver';
  end;
end;

end.
