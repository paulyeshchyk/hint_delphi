unit OPP.Buffer.Clipboard;

interface

uses
  System.SysUtils, System.Classes,
  WinAPI.Windows,
  Vcl.Clipbrd,
  OPP.Buffer.SLYK;

type

  TOPPBufferManagerRecord = class
  private
    fSLYK: TOPPBufferSLYKObject;
    fData: Variant;
    fIsFixed: Boolean;
    fSortIndex: Integer;
  public
    procedure SetText(AText: String);
    property SLYK: TOPPBufferSLYKObject read fSLYK write fSLYK;
    property Data: Variant read fData;
    property IsFixed: Boolean read fIsFixed write fIsFixed;
    property SortIndex: Integer read fSortIndex write fSortIndex;
  end;

  TOPPClipboardHelper = class helper for TClipboard
  public
    function CreateRecord(SLYK: TOPPBufferSLYKObject): TOPPBufferManagerRecord;
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

function TOPPClipboardHelper.CreateRecord(SLYK: TOPPBufferSLYKObject): TOPPBufferManagerRecord;
begin
  result := nil;

  try
    Clipboard.Open;
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      result := TOPPBufferManagerRecord.Create;
      result.SLYK := SLYK;
      result.SetText(Clipboard.AsText);
    end;
    Clipboard.Close;
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
