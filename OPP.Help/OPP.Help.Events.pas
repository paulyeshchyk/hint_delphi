unit OPP.Help.Events;

interface

uses System.Classes;

type
  TOPPHelpViewFullScreenSharedMessage = record
    page: Integer;
  end;

  TOPPHelpViewFullScreenSharedMessageHelper = record helper for TOPPHelpViewFullScreenSharedMessage
    function writeToStream(AStream: TStream): Boolean;
    function readFromStream(AStream: TStream): Boolean;
  end;

implementation

uses OPP.Help.System.Stream;

function TOPPHelpViewFullScreenSharedMessageHelper.writeToStream(AStream: TStream): Boolean;
begin
  result := false;
  if not assigned(AStream) then
    exit;
  AStream.WriteInteger(self.page);
  result := true;
end;

function TOPPHelpViewFullScreenSharedMessageHelper.readFromStream(AStream: TStream): Boolean;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  self.page := AStream.ReadInteger;
  result := true;

end;

end.
