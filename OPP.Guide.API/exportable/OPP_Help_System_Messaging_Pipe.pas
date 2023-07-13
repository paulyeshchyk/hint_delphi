unit OPP_Help_System_Messaging_Pipe;

interface

type
  TOPPHelpSystemMessagePipeWrapper = class
  public
    class procedure SendRecord(AReceiverHandle: THandle; ASenderHandle: THandle); static;
  end;

implementation

uses
  OPP.Help.System.Messaging.Pipe;

{ TOPPHelpSystemMessagePipeWrapper }

class procedure TOPPHelpSystemMessagePipeWrapper.SendRecord(AReceiverHandle, ASenderHandle: THandle);
begin
  //
end;

end.
