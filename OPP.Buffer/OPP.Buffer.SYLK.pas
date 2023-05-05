unit OPP.Buffer.SYLK;

interface

type
  TOPPBufferSYLKObjectType = (otObjControl = 0, otAttrControl = 1, otWinControl = 2);

  TOPPBufferSYLKObject = class
  private
    fOPPBufferType: TOPPBufferSYLKObjectType;
    fLoodsmanId: String;
    fLoodsmanType: String;
    fText: String;
  public
    constructor Create(AType: TOPPBufferSYLKObjectType; AText: String);
    procedure LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean = false);
    function SaveToBytes: TArray<Byte>;
    //
    property text: String read fText;
    property oppBufferType: TOPPBufferSYLKObjectType read fOPPBufferType write fOPPBufferType;
    property loodsmanType: String read fLoodsmanType write fLoodsmanType;
    property loodsmanId: String read fLoodsmanId write fLoodsmanId;
  end;

implementation

uses
  System.Classes, System.SysUtils,
  OPP.Help.System.JSON;

{ TOPPBufferSYLKObject }

constructor TOPPBufferSYLKObject.Create(AType: TOPPBufferSYLKObjectType; AText: String);
begin
  fOPPBufferType := AType;
  fText := AText;
end;

procedure TOPPBufferSYLKObject.LoadFromBytes(bytes: TArray<Byte>; isUTF8: Boolean);
begin
  TOPPJSONParser.deserialize<TOPPBufferSYLKObject>(bytes, isUTF8,
    procedure(AResult: TOPPBufferSYLKObject; Error: Exception)
    begin
    end);
end;

function TOPPBufferSYLKObject.SaveToBytes: TArray<Byte>;
var
  jsonString: String;
begin
  jsonString := TOPPJSONParser.Serialize<TOPPBufferSYLKObject>(self);
  result := TEncoding.UTF8.GetBytes(jsonString);
end;

end.
