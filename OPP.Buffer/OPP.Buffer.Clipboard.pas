unit OPP.Buffer.Clipboard;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Windows,
  Vcl.Clipbrd,
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
begin
  result := nil;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      result := TOPPBufferManagerRecord.Create;
      result.OPPInfo := OPPInfo;
      result.text := Clipboard.AsText;
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
