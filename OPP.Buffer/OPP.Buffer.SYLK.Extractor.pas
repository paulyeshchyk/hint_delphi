unit OPP.Buffer.SYLK.Extractor;

interface

uses System.Classes, Vcl.Controls, System.TypInfo, System.SysUtils,
  Datasnap.dbclient,
  OPP.Buffer.SYLK, OPP.Help.Component.Enumerator;

type
  TOPPBufferSYLKExtractor = class
  public
    class function GetSYLK(Sender: TObject): TOPPBufferSYLKObject;
    class procedure SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TObject);
  end;

implementation

uses
  OppObjControl,
  OppAttrControl,
  OPPRTTIUtils, OPPHelpers;

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

class procedure TOPPBufferSYLKExtractor.SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TObject);
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
begin
  if (not(AControl is TWinControl)) or (not Assigned(ASYLK)) then
    exit;

  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(AControl as TWinControl);

  if Assigned(foppObjControl) then
  begin
    if (ASYLK.oppBufferType = otObjControl) then
    begin
      try
        foppObjControl.ObjectID := StrToInt(ASYLK.loodsmanId);
      except
        foppObjControl.ObjectID := 0;
      end;
    end else begin
    end;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(AControl as TWinControl);
  if Assigned(foppAttrControl) then
  begin
    if ASYLK.oppBufferType = otAttrControl then
    begin
      foppAttrControl.DataInControl := ASYLK.loodsmanId;
    end else begin
    end;
    // if foppAttrControl.isTypeAcceptable(TAttrKind.FromIntValue(StrToInt(ASYLK.loodsmanType))) then
    // begin
    // end;
    exit;
  end;

end;

{ TOPPBufferObjControlHelper }

function TOPPBufferObjControlHelper.GetLoodsmanType: String;
begin
  result := self.TypeObject;
end;

function TOPPBufferObjControlHelper.isTypeAcceptable(AType: String): Boolean;
begin
  result := (CompareStr(UpperCase(self.loodsmanType), UpperCase(AType)) = 0);
end;

function TOPPBufferObjControlHelper.sylkObject: TOPPBufferSYLKObject;
begin
  result := TOPPBufferSYLKObject.Create(otObjControl);
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
  result := TOPPBufferSYLKObject.Create(otAttrControl);
  result.loodsmanType := Format('%d', [TAttrKind.IntValue(self.AttrKind)]);
  result.loodsmanId := self.DataInControl;
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.sylkObject: TOPPBufferSYLKObject;
begin
  result := TOPPBufferSYLKObject.Create(otWinControl);
  result.loodsmanType := '';
  result.loodsmanId := OPPRTTIUtils.OPPObjectDOTPropertyValueGet(self, 'EditValue');
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
