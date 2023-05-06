unit OPP.Buffer.Clipboard;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Windows,
  Vcl.Clipbrd,
  OPP.Buffer.SYLK;

type

  TOPPBufferManagerRecord = class
  private
    fSYLK: TOPPBufferSYLKObject;
    fData: Variant;
    fIsFixed: Boolean;
    fSortIndex: Integer;
  public
    procedure SetText(AText: String);
    property SYLK: TOPPBufferSYLKObject read fSYLK write fSYLK;
    property Data: Variant read fData;
    property IsFixed: Boolean read fIsFixed write fIsFixed;
    property SortIndex: Integer read fSortIndex write fSortIndex;
  end;

  TOPPClipboardHelper = class helper for TClipboard
  public
    function CreateRecord(SYLK: TOPPBufferSYLKObject): TOPPBufferManagerRecord;
    function HasClipboardFormat(): Boolean;
  end;

implementation

uses
  OPP.Help.Log;

{ TOPPBufferManagerRecord }

procedure TOPPBufferManagerRecord.SetText(AText: String);
begin
  fData := AText;
end;

{ TOPPClipboardHelper }

function TOPPClipboardHelper.CreateRecord(SYLK: TOPPBufferSYLKObject): TOPPBufferManagerRecord;
begin
  result := nil;

  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      result := TOPPBufferManagerRecord.Create;
      result.SYLK := SYLK;
      result.SetText(Clipboard.AsText);
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'Cliboard');
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
