unit OPP.Buffer.Manager.DatasetRecord;

interface

type
  TOPPBufferOPPInfo = class;

  TOPPBufferManagerRecord = class
  private
    fOPPInfo: TOPPBufferOPPInfo;
    fText: Variant;
    fIsFixed: Boolean;
    fSortIndex: Integer;
    procedure SetText(const Value: Variant);
  public
    property OPPInfo: TOPPBufferOPPInfo read fOPPInfo write fOPPInfo;
    property Text: Variant read fText write SetText;
    property IsFixed: Boolean read fIsFixed write fIsFixed;
    property SortIndex: Integer read fSortIndex write fSortIndex;
  end;

  TOPPBufferOPPInfoType = (otObjControl, otAttrControl, otWinControl);

  TOPPBufferOPPInfo = class
  private
    fOPPBufferType: TOPPBufferOPPInfoType;
    fLoodsmanId: String;
    fLoodsmanType: String;
    fLoodsmanAttribute: String;
  public
    constructor Create(AType: TOPPBufferOPPInfoType);

    property oppBufferType: TOPPBufferOPPInfoType read fOPPBufferType write fOPPBufferType;
    property loodsmanAttribute: String read fLoodsmanAttribute write fLoodsmanAttribute;
    property loodsmanType: String read fLoodsmanType write fLoodsmanType;
    property loodsmanId: String read fLoodsmanId write fLoodsmanId;
  end;


implementation

{ TOPPBufferOPPInfo }

constructor TOPPBufferOPPInfo.Create(AType: TOPPBufferOPPInfoType);
begin
  fOPPBufferType := AType;
  fLoodsmanId := '';
  fLoodsmanType := '';
  fLoodsmanAttribute := '';
end;

{ TOPPBufferManagerRecord }

procedure TOPPBufferManagerRecord.SetText(const Value: Variant);
begin
  fText := Value;
end;

end.
