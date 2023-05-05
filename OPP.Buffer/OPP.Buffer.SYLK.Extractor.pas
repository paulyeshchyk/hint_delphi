unit OPP.Buffer.SYLK.Extractor;

interface

uses System.Classes, Vcl.Controls, System.TypInfo, System.SysUtils,
  Datasnap.dbclient, WinAPI.Messages,
  OPP.Buffer.SYLK, OPP.Help.Component.Enumerator;

type
  TOPPBufferSYLKExtractor = class
  public
    class function GetSYLK(Sender: TObject): TOPPBufferSYLKObject;
    class procedure SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TWinControl);
  end;

implementation

uses
  Clipbrd, WinAPI.Windows,
  OppObjControl,
  OppAttrControl,
  OPPRTTIUtils, OPPHelpers, OPP.Help.System.Str;

type

  TOPPBufferObjControlHelper = class helper for TOppObjControl
  private
    function GetLoodsmanType: String;
  public
    function sylkObject: TOPPBufferSYLKObject;
    function isTypeAcceptable(AType: String): Boolean;
    property loodsmanType: String read GetLoodsmanType;
  end;

  TOPPBufferAttrControlHelper = class helper for TOppAttrControl
    function sylkObject: TOPPBufferSYLKObject;
    function isTypeAcceptable(AType: TAttrKind): Boolean;
  end;

  TOPPBufferWinControlHelper = class helper for TWinControl
    function sylkObject: TOPPBufferSYLKObject;
  end;

  TAttrKindHelper = record helper for TAttrKind
    class function FromIntValue(AValue: Integer): TAttrKind; static;
    class function IntValue(AAttrKind: TAttrKind): Integer; static;
  end;

  { TOPPBufferSYLKExtractor }

class function TOPPBufferSYLKExtractor.GetSYLK(Sender: TObject): TOPPBufferSYLKObject;
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
begin

  result := nil;

  if not(Sender is TWinControl) then
    exit;

  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(Sender as TWinControl);

  if Assigned(foppObjControl) then
  begin
    result := foppObjControl.sylkObject;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(Sender as TWinControl);
  if Assigned(foppAttrControl) then
  begin
    result := foppAttrControl.sylkObject;
    exit;
  end;

  result := (Sender as TWinControl).sylkObject;
end;

class procedure TOPPBufferSYLKExtractor.SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TWinControl);
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
  fTextBuff: PWideChar;
begin
  if (not(AControl is TWinControl)) or (not Assigned(ASYLK)) then
    exit;

  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(AControl);

  if Assigned(foppObjControl) then
  begin
    if (CompareStr(Uppercase(ASYLK.loodsmanType), Uppercase(foppObjControl.TypeObject)) = 0) then
    begin
      try
        foppObjControl.ObjectID := StrToInt(ASYLK.loodsmanId);
      except
        foppObjControl.ObjectID := 0;
      end;
    end else begin
      foppObjControl.DataInControl := ASYLK.text;
    end;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(AControl);
  if Assigned(foppAttrControl) then
  begin
    if ASYLK.oppBufferType = otAttrControl then
    begin
      foppAttrControl.DataInControl := ASYLK.loodsmanId;
    end else begin
      foppAttrControl.DataInControl := ASYLK.text;
    end;
    exit;
  end;

  fTextBuff := ASYLK.text.ToWideChar;
  try
    try
      SendMessage(AControl.Handle, WM_SETTEXT, 0, LParam(fTextBuff));
    except
    end;
  finally
    FreeMem(fTextBuff);
  end;

end;

{ TOPPBufferObjControlHelper }

function TOPPBufferObjControlHelper.GetLoodsmanType: String;
begin
  result := self.TypeObject;
end;

function TOPPBufferObjControlHelper.isTypeAcceptable(AType: String): Boolean;
begin
  result := (CompareStr(Uppercase(self.loodsmanType), Uppercase(AType)) = 0);
end;

function TOPPBufferObjControlHelper.sylkObject: TOPPBufferSYLKObject;
begin
  result := TOPPBufferSYLKObject.Create(otObjControl, self.DataInControl);
  result.loodsmanType := self.loodsmanType;
  result.loodsmanId := Format('%d', [self.ObjectID]);
end;

{ TOPPBufferAttrControlHelper }

function TOPPBufferAttrControlHelper.isTypeAcceptable(AType: TAttrKind): Boolean;
begin
  result := (self.AttrKind = AType);
end;

function TOPPBufferAttrControlHelper.sylkObject: TOPPBufferSYLKObject;
begin
  result := TOPPBufferSYLKObject.Create(otAttrControl, self.DataInControl);
  result.loodsmanType := Format('%d', [TAttrKind.IntValue(self.AttrKind)]);
  result.loodsmanId := self.DataInControl;
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.sylkObject: TOPPBufferSYLKObject;
var
  fText: PWideChar;
  fLength: Integer;
  fString: String;
begin
  fLength := SendMessage(self.Handle, WM_GETTEXTLENGTH, 0, 0);
  if fLength = 0 then
    exit;

  GetMem(fText, fLength + 1);
  try
    try
      SendMessage(self.Handle, WM_GETTEXT, fLength + 1 , LParam(fText));

      fString := WideCharToString(fText);
      result := TOPPBufferSYLKObject.Create(otWinControl, fString);
      result.loodsmanType := '';
      result.loodsmanId := '';
    except
    end;
  finally
    FreeMem(fText);
  end;
end;

{ TAttrKindHelper }

class function TAttrKindHelper.FromIntValue(AValue: Integer): TAttrKind;
begin
  case AValue of
    1:
      result := akString;
    2:
      result := akInteger;
    3:
      result := akFloat;
    4:
      result := akDateTime;
    5:
      result := akText;
    6:
      result := akImage;
  else
    result := akUndefined;
  end;
end;

class function TAttrKindHelper.IntValue(AAttrKind: TAttrKind): Integer;
begin
  // (akUndefined, akString, akInteger, akFloat, akDateTime, akText, akImage);

  case AAttrKind of
    akUndefined:
      result := 0;
    akString:
      result := 1;
    akInteger:
      result := 2;
    akFloat:
      result := 3;
    akDateTime:
      result := 4;
    akText:
      result := 5;
    akImage:
      result := 6;
  end;
end;

end.
