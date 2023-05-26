unit OPP.Buffer.OPPInfo;

interface

type

  TOPPBufferOPPInfoType = (otObjControl, otAttrControl, otWinControl);

  TOPPBufferOPPInfo = class
  private
    fOPPBufferType: TOPPBufferOPPInfoType;
    fLoodsmanId: String;
    fLoodsmanType: String;
    fLoodsmanAttribute: String;
    fControlText: String;
  public
    constructor Create(AType: TOPPBufferOPPInfoType);
    procedure LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean);
    function SaveToBytes: TArray<Byte>;

    property oppBufferType: TOPPBufferOPPInfoType read fOPPBufferType write fOPPBufferType;
    property loodsmanAttribute: String read fLoodsmanAttribute write fLoodsmanAttribute;
    property loodsmanType: String read fLoodsmanType write fLoodsmanType;
    property loodsmanId: String read fLoodsmanId write fLoodsmanId;
    property ControlText: String read fControlText write fControlText;
  end;

  TOPPBufferOPPInfoDebugHelper = class helper for TOPPBufferOPPInfo
  private
    function GetDebugInfo: String;
  public
    property debugInfo: String read GetDebugInfo;
  end;

implementation

uses
  System.SysUtils, System.Classes,
  OPP.Help.System.JSON,
  OPP.Help.Log;

{ TOPPBufferOPPInfo }

constructor TOPPBufferOPPInfo.Create(AType: TOPPBufferOPPInfoType);
begin
  fOPPBufferType := AType;
  fLoodsmanId := '';
  fLoodsmanType := '';
  fLoodsmanAttribute := '';
  fControlText := '';
end;

procedure TOPPBufferOPPInfo.LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean);
begin
  TOPPJSONParser.DeSerialize<TOPPBufferOPPInfo>(bytes, isUTF8,
    procedure(AResult: TOPPBufferOPPInfo; Error: Exception)
    begin
    end);
end;

function TOPPBufferOPPInfo.SaveToBytes: TArray<Byte>;
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
      eventLogger.Error(E, 'TOPPBufferOPPInfo');
    end;
  end;
end;

{ TOPPBufferOPPInfoDebugHelper }

function TOPPBufferOPPInfoDebugHelper.GetDebugInfo: String;
var
  strings: TStringList;
begin
  strings := TStringList.Create;
  try
    strings.Add(Format('%s=`%s`',['loodsmanAttribute',loodsmanAttribute]));
    strings.Add(Format('%s=`%s`',['loodsmanType',loodsmanType]));
    strings.Add(Format('%s=`%s`',['loodsmanId',loodsmanId]));
    strings.Add(Format('%s=`%s`',['ControlText',ControlText]));
    strings.Add(Format('%s=`%d`',['OPPBufferType',Integer(fOPPBufferType)]));
    strings.Delimiter := ';';
    result := strings.DelimitedText;
  finally
    strings.Free;
  end;

end;

end.
