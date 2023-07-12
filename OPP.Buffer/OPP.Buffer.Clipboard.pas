unit OPP.Buffer.Clipboard;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Windows,
  Vcl.Clipbrd,
  OPP.Buffer.OPPInfo,
  OPP.Buffer.Manager.DatasetRecord;

type

  TOPPClipboardHelper = class helper for TClipboard
  private
    function CF_DefaultRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
    function CF_TEXTRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
  public
    function CreateRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
    function HasClipboardFormat(): Boolean;
  end;

implementation

uses
  OPP.Help.Log;

{ TOPPClipboardHelper }

function TOPPClipboardHelper.CF_DefaultRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
begin
  result := nil;
  if not Assigned(OPPInfo) then
    exit;
  if Length(OPPInfo.ControlText) = 0 then
    exit;
  result := TOPPBufferManagerRecord.Create;
  result.OPPInfo := OPPInfo;
  result.text := OPPInfo.ControlText;
end;

function TOPPClipboardHelper.CF_TEXTRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
var
  fText: String;
begin
  result := nil;
  fText := Clipboard.AsText;
  if Length(fText) = 0 then
  begin
    eventLogger.Warning('Clipboard[CF_TEXT] contains no text', 'TOPPClipboardHelper');
    exit;
  end;

  result := TOPPBufferManagerRecord.Create;
  result.OPPInfo := OPPInfo;
  result.text := fText;
end;

function TOPPClipboardHelper.CreateRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
begin
  result := self.CF_DefaultRecord(OPPInfo);
  if Assigned(result) then
    exit;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      result := self.CF_TEXTRecord(OPPInfo);
    end else begin
      eventLogger.Warning('Incompatible clipboard format', 'TOPPClipboardHelper');
    end;

  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'TOPPClipboardHelper');
    end;
  end;

end;

function TOPPClipboardHelper.HasClipboardFormat(): Boolean;
begin
  try
    Clipboard.open;
    result := Clipboard.HasFormat(CF_TEXT);
    Clipboard.Close;
  except
    result := false;
  end;
end;

end.
