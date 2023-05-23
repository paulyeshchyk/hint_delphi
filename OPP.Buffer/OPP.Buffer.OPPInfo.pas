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

implementation

uses
  System.SysUtils,
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

end.
