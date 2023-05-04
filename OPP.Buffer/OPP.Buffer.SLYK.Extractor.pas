unit OPP.Buffer.SLYK.Extractor;

interface

uses System.Classes, Vcl.Controls, System.TypInfo, System.SysUtils,
  Datasnap.dbclient,
  OPP.Buffer.SLYK;

type
  TParentExtractor<T: class> = class
    class function GetParent(Sender: TWinControl): T;
  end;

  TOPPBufferSLYKExtractor = class
  public
    class function GetSLYK(Sender: TObject): TOPPBufferSLYKObject;
  end;

implementation

uses
  OppObjControl,
  OppAttrControl,
  OPPRTTIUtils, OPPHelpers;

type
  TOPPBufferObjControlHelper = class helper for TOppObjControl
    function slykObject: TOPPBufferSLYKObject;
  end;

  TOPPBufferAttrControlHelper = class helper for TOppAttrControl
    function slykObject: TOPPBufferSLYKObject;
  end;

  TOPPBufferWinControlHelper = class helper for TWinControl
    function slykObject: TOPPBufferSLYKObject;
  end;

  TAttrKindHelper = record helper for TAttrKind
    function fromIntValue(AValue: Integer): TAttrKind;
    function IntValue: Integer;
  end;

  { TOPPBufferSLYKExtractor }

class function TOPPBufferSLYKExtractor.GetSLYK(Sender: TObject): TOPPBufferSLYKObject;
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
begin

  result := nil;

  if not(Sender is TWinControl) then
    exit;

  foppObjControl := TParentExtractor<TOppObjControl>.GetParent(Sender as TWinControl);

  if Assigned(foppObjControl) then
  begin
    result := foppObjControl.slykObject;
    exit;
  end;

  foppAttrControl := TParentExtractor<TOppAttrControl>.GetParent(Sender as TWinControl);
  if Assigned(foppAttrControl) then
  begin
    result := foppAttrControl.slykObject;
    exit;
  end;

  result := (Sender as TWinControl).slykObject;

end;

{ TParentExtractor<T> }

class function TParentExtractor<T>.GetParent(Sender: TWinControl): T;
var
  pTypeInfo: System.TypInfo.pTypeInfo;
begin
  result := nil;
  if not Assigned(Sender) then
    exit;

  pTypeInfo := System.TypeInfo(T);

  if pTypeInfo^.Name = Sender.ClassName then
  begin
    result := Sender as T;
    exit;
  end;

  result := self.GetParent(Sender.parent);

end;

{ TOPPBufferObjControlHelper }

function TOPPBufferObjControlHelper.slykObject: TOPPBufferSLYKObject;
var
  data, inret, stret: Variant;
  cds: TClientDataSet;
begin
  result := nil;
  data := self.Config.PLM1.GetInfoAboutVersion('', '', '', self.ObjectID, 15, inret, stret);
  if (inret = 0) then
  begin
    cds := TClientDataSet.Create(nil);
    try
      cds.data := data;
      cds.first;
      if not cds.eof then
      begin
        result := TOPPBufferSLYKObject.Create(otObjControl);
        result.loodsmanType := cds.FieldByName('_TYPE').asString;
        result.loodsmanId := Format('%d', [self.ObjectID]);
      end;
    finally
      cds.Free;
    end;
  end;

end;

{ TOPPBufferAttrControlHelper }

function TOPPBufferAttrControlHelper.slykObject: TOPPBufferSLYKObject;
begin
  result := TOPPBufferSLYKObject.Create(otAttrControl);
  result.loodsmanType := Format('%d', [self.AttrKind.IntValue]);
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.slykObject: TOPPBufferSLYKObject;
begin
  result := TOPPBufferSLYKObject.Create(otWinControl);
  result.loodsmanType := '';
  result.loodsmanId := OPPRTTIUtils.OPPObjectDOTPropertyValueGet(self, 'EditValue');
end;

{ TAttrKindHelper }

function TAttrKindHelper.fromIntValue(AValue: Integer): TAttrKind;
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

function TAttrKindHelper.IntValue: Integer;
begin
  // (akUndefined, akString, akInteger, akFloat, akDateTime, akText, akImage);

  case self of
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
