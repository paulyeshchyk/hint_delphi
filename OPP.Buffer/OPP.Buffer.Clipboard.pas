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
  public
    function CreateRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
    function HasClipboardFormat(): Boolean;
  end;

implementation

uses
  OPP.Help.Log;

const
  kContext = 'TOPPClipboardHelper';

{ TOPPClipboardHelper }

function TOPPClipboardHelper.CreateRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
var
  fText: String;
begin
  eventLogger.Flow('CreateRecord', kContext);

  result := nil;
  if not Assigned(OPPInfo) then
  begin
    eventLogger.Warning('OPPInfo is not defined', kContext);
    exit;
  end;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      fText := Clipboard.AsText;
      if length(fText) = 0 then
      begin
        eventLogger.Warning('Clipboard[CF_TEXT] contains no text', kContext);
        fText := OPPInfo.ControlText;
      end;

      result := TOPPBufferManagerRecord.Create;
      result.OPPInfo := OPPInfo;
      result.text := fText;
    end
    else if Clipboard.HasFormat(CF_LOCALE) then
    begin
      fText := Clipboard.AsText;
      if length(fText) = 0 then
      begin
        eventLogger.Warning('Clipboard[CF_LOCALE] contains no text', kContext);
        fText := OPPInfo.ControlText;
      end;
      result := TOPPBufferManagerRecord.Create;
      result.OPPInfo := OPPInfo;
      result.text := fText;
    end;

    if not Assigned(result) then
    begin
      eventLogger.Warning(Format('not created record for OPPInfo: %s', [OPPInfo.debugInfo]), kContext);
    end else begin
      eventLogger.Flow(Format('Created record for OPPInfo: %s', [OPPInfo.debugInfo]), kContext);
    end;

  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;

end;

function TOPPClipboardHelper.HasClipboardFormat(): Boolean;
begin
  try
    Clipboard.Open;
    result := Clipboard.HasFormat(CF_TEXT);
    Clipboard.Close;
  except
    result := false;
  end;
end;

end.
