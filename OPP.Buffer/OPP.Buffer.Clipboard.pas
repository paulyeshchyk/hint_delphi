unit OPP.Buffer.Clipboard;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Windows,
  Vcl.Clipbrd;

type

  TOPPBufferManagerItemFormat = (ifText, ifBitmap);

  TOPPBufferManagerRecord = class
  private
    fFormat: TOPPBufferManagerItemFormat;
    fData: Variant;
    fIsFixed: Boolean;
    fSortIndex: Integer;
  public
    procedure SetText(AText: String);
    property Format: TOPPBufferManagerItemFormat read fFormat;
    property Data: Variant read fData;
    property IsFixed: Boolean read fIsFixed write fIsFixed;
    property SortIndex: Integer read fSortIndex write fSortIndex;
  end;

  TSLYKHelper = class helper for TStringList
  public
    function CreateRecord(AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
  end;

  TOPPClipboardHelper = class helper for TClipboard
  public
    function CreateRecord(AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
    function HasClipboardFormat(AFormat: TOPPBufferManagerItemFormat = ifText): Boolean;
  end;

  TOPPBufferManagerItemFormatHelper = record helper for TOPPBufferManagerItemFormat
  public
    function WindowsClipboardFormat: Word;
    class function FromWindowsClipboardFormat(AWindowsClipboardFormat: Word): TOPPBufferManagerItemFormat; static;
  end;

implementation

uses
  OPP.Help.Log;

{ TOPPBufferManagerRecord }

procedure TOPPBufferManagerRecord.SetText(AText: String);
begin
  fData := AText;
  fFormat := ifText;
end;

{ TOPPClipboardHelper }

function TOPPClipboardHelper.CreateRecord(AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
var
  fWindowsClipboardType: Word;
begin
  result := nil;
  fWindowsClipboardType := AFormat.WindowsClipboardFormat;

  try
    Clipboard.Open;
    if Clipboard.HasFormat(fWindowsClipboardType) then
    begin
      case fWindowsClipboardType of
        CF_TEXT:
          begin
            result := TOPPBufferManagerRecord.Create;
            result.SetText(Clipboard.AsText);
          end;
      end;
    end;
    Clipboard.Close;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'Cliboard');
    end;
  end;

end;

function TOPPClipboardHelper.HasClipboardFormat(AFormat: TOPPBufferManagerItemFormat): Boolean;
begin
  try
    Clipboard.Open;
    result := Clipboard.HasFormat(AFormat.WindowsClipboardFormat);
    Clipboard.Close;
  except
    result := false;
  end;
end;

{ TOPPBufferManagerItemFormatHelper }

class function TOPPBufferManagerItemFormatHelper.FromWindowsClipboardFormat(AWindowsClipboardFormat: Word): TOPPBufferManagerItemFormat;
begin
  case AWindowsClipboardFormat of
    CF_TEXT:
      result := ifText;
    CF_BITMAP:
      result := ifBitmap;
  else
    result := ifText;
  end;
end;

function TOPPBufferManagerItemFormatHelper.WindowsClipboardFormat: Word;
begin
  case self of
    ifText:
      result := CF_TEXT;
    ifBitmap:
      result := CF_BITMAP;
  else
    result := CF_TEXT;
  end;
end;

{ TSLYKHelper }

function TSLYKHelper.CreateRecord(AFormat: TOPPBufferManagerItemFormat): TOPPBufferManagerRecord;
begin
//
end;

end.
