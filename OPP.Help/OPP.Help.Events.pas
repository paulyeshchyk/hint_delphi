unit OPP.Help.Events;

interface

uses System.Classes;

type
  TOPPHelpViewFullScreenSharedMessage = record
    page: Integer;
    filename: String;
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
  AStream.WriteString(self.filename);
  result := true;
end;

function TOPPHelpViewFullScreenSharedMessageHelper.readFromStream(AStream: TStream): Boolean;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  self.page := AStream.ReadInteger;
  self.filename := AStream.ReadString;
  result := true;

end;

end.
