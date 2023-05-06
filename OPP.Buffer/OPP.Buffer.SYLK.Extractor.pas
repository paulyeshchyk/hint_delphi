unit OPP.Buffer.SYLK.Extractor;

interface

uses System.Classes, Vcl.Controls, System.TypInfo, System.SysUtils,
  Datasnap.dbclient, WinAPI.Messages,
  OPP.Buffer.SYLK, OPP.Help.Component.Enumerator;

type
  TOPPBufferSYLKExtractor = class
  public
    class function GetSYLK(Sender: TWinControl): TOPPBufferSYLKObject;
    class procedure SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TWinControl);
  end;

implementation

uses
  Clipbrd, WinAPI.Windows,
  OppObjControl,
  OppAttrControl,
  OPP.Help.System.Control,
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
  end;

  TOPPBufferWinControlHelper = class helper for TWinControl
    function sylkObject: TOPPBufferSYLKObject;
  end;

  { TOPPBufferSYLKExtractor }

class function TOPPBufferSYLKExtractor.GetSYLK(Sender: TWinControl): TOPPBufferSYLKObject;
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
begin

  result := nil;

  if not Assigned(Sender) then
    exit;

  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(Sender);

  if Assigned(foppObjControl) then
  begin
    result := foppObjControl.sylkObject;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(Sender);
  if Assigned(foppAttrControl) then
  begin
    result := foppAttrControl.sylkObject;
    exit;
  end;

  result := Sender.sylkObject;
end;

class procedure TOPPBufferSYLKExtractor.SetSYLK(ASYLK: TOPPBufferSYLKObject; AControl: TWinControl);
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
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

  AControl.SetTextPropertyValue(ASYLK.text);
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

function TOPPBufferAttrControlHelper.sylkObject: TOPPBufferSYLKObject;
begin
  result := TOPPBufferSYLKObject.Create(otAttrControl, self.DataInControl);
  result.loodsmanType := '';
  result.loodsmanId := self.DataInControl;
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.sylkObject: TOPPBufferSYLKObject;
var
  fText: PWideChar;
  fLength: Integer;
  fString: String;
begin
  result := nil;
  if not(self is TWinControl) then
    exit;
  fString := self.TextPropertyValue;
  result := TOPPBufferSYLKObject.Create(otWinControl, fString);
  result.loodsmanType := '';
  result.loodsmanId := '';
end;

end.
