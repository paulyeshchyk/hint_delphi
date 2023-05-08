unit OPP.Buffer.OPPInfo.Helper;

interface

uses
  System.Classes, System.TypInfo, System.SysUtils,
  Vcl.Controls,
  Datasnap.dbclient, WinAPI.Messages,
  OPP.Buffer.Manager.DatasetRecord,
  OPP.Help.Component.Enumerator;

type

  TOPPBufferOPPInfoHelper = class helper for TOPPBufferOPPInfo
  public
    procedure LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean);
    function SaveToBytes: TArray<Byte>;

    class function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
    class procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
  end;

implementation

uses
  Clipbrd, WinAPI.Windows,
  OppObjControl,
  OppAttrControl,
  OPP.Help.System.Control,
  OPP.Help.System.JSON,
  OPP.Help.Log,
  OPPRTTIUtils, OPPHelpers, OPP.Help.System.Str;

type

  TOPPBufferObjControlHelper = class helper for TOppObjControl
  private
    function GetLoodsmanType: String;
  public
    function OPPInfo: TOPPBufferOPPInfo;
    function isTypeAcceptable(AType: String): Boolean;
    property loodsmanType: String read GetLoodsmanType;
  end;

  TOPPBufferAttrControlHelper = class helper for TOppAttrControl
    function OPPInfo: TOPPBufferOPPInfo;
  end;

  TOPPBufferWinControlHelper = class helper for TWinControl
    function OPPInfo: TOPPBufferOPPInfo;
  end;

procedure TOPPBufferOPPInfoHelper.LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean);
begin
  TOPPJSONParser.deserialize<TOPPBufferOPPInfo>(bytes, isUTF8,
    procedure(AResult: TOPPBufferOPPInfo; Error: Exception)
    begin
    end);
end;

function TOPPBufferOPPInfoHelper.SaveToBytes: TArray<Byte>;
var
  jsonString: String;
begin
  try
    jsonString := TOPPJSONParser.Serialize<TOPPBufferOPPInfo>(self);
    result := TEncoding.UTF8.GetBytes(jsonString);
  except
    on E: Exception do
    begin
      result := nil;
      eventLogger.Error(E, 'OPPInfo');
    end;
  end;
end;

class function TOPPBufferOPPInfoHelper.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
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
    result := foppObjControl.OPPInfo;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(Sender);
  if Assigned(foppAttrControl) then
  begin
    result := foppAttrControl.OPPInfo;
    exit;
  end;

  result := Sender.OPPInfo;
end;

class procedure TOPPBufferOPPInfoHelper.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
var
  foppObjControl: TOppObjControl;
  foppAttrControl: TOppAttrControl;
begin
  if (not(AControl is TWinControl)) or (not Assigned(OPPInfo)) then
    exit;

  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(AControl);

  if Assigned(foppObjControl) then
  begin
    if (CompareStr(Uppercase(OPPInfo.loodsmanType), Uppercase(foppObjControl.TypeObject)) = 0) then
    begin
      try
        foppObjControl.ObjectID := StrToInt(OPPInfo.loodsmanId);
      except
        foppObjControl.ObjectID := 0;
      end;
    end else begin
      foppObjControl.DataInControl := AText;
    end;
    exit;
  end;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(AControl);
  if Assigned(foppAttrControl) then
  begin
    if OPPInfo.oppBufferType = otAttrControl then
    begin
      foppAttrControl.DataInControl := OPPInfo.loodsmanId;
    end else begin
      foppAttrControl.DataInControl := AText;
    end;
    exit;
  end;

  AControl.SetTextPropertyValue(AText);
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

function TOPPBufferObjControlHelper.OPPInfo: TOPPBufferOPPInfo;
begin
  result := TOPPBufferOPPInfo.Create(otObjControl);
  result.loodsmanType := self.loodsmanType;
  result.loodsmanId := Format('%d', [self.ObjectID]);
  result.loodsmanAttribute := self.Attribute;
end;

{ TOPPBufferAttrControlHelper }

function TOPPBufferAttrControlHelper.OPPInfo: TOPPBufferOPPInfo;
begin
  result := TOPPBufferOPPInfo.Create(otAttrControl);
  result.loodsmanType := '';
  result.loodsmanId := self.DataInControl;
  result.loodsmanAttribute := self.Attribute;
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.OPPInfo: TOPPBufferOPPInfo;
var
  fText: PWideChar;
  fLength: Integer;
begin
  result := nil;
  if not(self is TWinControl) then
    exit;

  result := TOPPBufferOPPInfo.Create(otWinControl);
end;

end.
