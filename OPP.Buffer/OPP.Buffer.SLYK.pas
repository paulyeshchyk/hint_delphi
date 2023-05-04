unit OPP.Buffer.SLYK;

interface

type
  TOPPBufferSLYKObjectType = (otObjControl = 0, otAttrControl = 1, otWinControl = 2);


  TOPPBufferSLYKObject = class
  private
    fOPPBufferType: TOPPBufferSLYKObjectType;
    fLoodsmanId: String;
    fLoodsmanType: String;
  public
    constructor Create(AType: TOPPBufferSLYKObjectType);
    property oppBufferType: TOPPBufferSLYKObjectType read fOPPBufferType write fOPPBufferType;
    property loodsmanType: String read fLoodsmanType write fLoodsmanType;
    property loodsmanId: String read fLoodsmanId write fLoodsmanId;
  end;


implementation


{ TOPPBufferSLYKObject }

constructor TOPPBufferSLYKObject.Create(AType: TOPPBufferSLYKObjectType);
begin
  self.oppBufferType := AType;
end;

end.

