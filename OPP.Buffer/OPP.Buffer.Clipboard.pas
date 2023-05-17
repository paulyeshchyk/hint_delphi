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

{ TOPPClipboardHelper }

function TOPPClipboardHelper.CreateRecord(OPPInfo: TOPPBufferOPPInfo): TOPPBufferManagerRecord;
var
  fText: String;
begin
  result := nil;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      fText := Clipboard.AsText;
      if length(fText) = 0 then begin
        eventLogger.Warning('Clipboard[CF_TEXT] contains no text','TOPPClipboardHelper');
        fText := OPPInfo.ControlText;
      end;

      result := TOPPBufferManagerRecord.Create;
      result.OPPInfo := OPPInfo;
      result.text := fText;
    end
    else if Clipboard.HasFormat(CF_LOCALE) then
    begin
      fText := Clipboard.AsText;
      if Length(fText) = 0 then begin
        eventLogger.Warning('Clipboard[CF_LOCALE] contains no text','TOPPClipboardHelper');
        fText := OPPInfo.ControlText;
      end;
      result := TOPPBufferManagerRecord.Create;
      result.OPPInfo := OPPInfo;
      result.text := fText;
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
    Clipboard.Open;
    result := Clipboard.HasFormat(CF_TEXT);
    Clipboard.Close;
  except
    result := false;
  end;
end;

end.
