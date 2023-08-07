unit OPP.Price.Order;

interface

uses
  System.Generics.Collections,
  OPP.Price.Item;

type

  TOPPPriceOrderItem = class;

  TOPPPriceOrder = class
  private
    fItemList: TList<TOPPPriceOrderItem>;
    fName: String;
  public
    constructor Create(AName: String);
    destructor Destroy;override;
    procedure AddItem(AOrderItem: TOPPPriceOrderItem);
    property Name: String read fName;
    property ItemList: TList<TOPPPriceOrderItem> read fItemList;
  end;

  TOPPPriceOrderItem = class
  private
    fPriceItem: TOPPPriceItem;
    fSumm: Extended;
    fGuid: TGUID;
    function GetIdentificator: String;
  public
    constructor Create(APriceItem: TOPPPriceItem; ASumm: Extended);
    property PriceItem: TOPPPriceItem read fPriceItem;
    property Summ: Extended read fSumm;
    property Identificator: String read GetIdentificator;
  end;

  TOPPPriceOrderDefaults = class
  public
    class function GetDefaultList: TList<TOPPPriceOrder>;static;
  end;

implementation

uses
  System.SysUtils;

{ TOPPPriceOrder }

procedure TOPPPriceOrder.AddItem(AOrderItem: TOPPPriceOrderItem);
begin
  if not Assigned(AOrderItem) then
    exit;
  fItemList.Add(AOrderItem);
end;

constructor TOPPPriceOrder.Create(AName: String);
begin
  inherited Create;
  fItemList := TList<TOPPPriceOrderItem>.Create;

  fName := AName;
end;

destructor TOPPPriceOrder.Destroy;
begin
  fItemList.Clear;
  fItemList.Free;
  inherited;
end;

{ TOPPPriceOrderItem }

constructor TOPPPriceOrderItem.Create(APriceItem: TOPPPriceItem; ASumm: Extended);
begin
  inherited Create;
  fPriceItem := APriceItem;
  fSumm := ASumm;
  CreateGUID(fGUID);
end;

function TOPPPriceOrderItem.GetIdentificator: String;
begin
  result := GUIDToString(fGUID);
end;

{ TOPPPriceOrderDefaults }

class function TOPPPriceOrderDefaults.GetDefaultList: TList<TOPPPriceOrder>;
var
  fOrder: TOPPPriceOrder;
  fOrderItem: TOPPPriceOrderItem;
begin
  result := TList<TOPPPriceOrder>.Create;

  fOrder := TOPPPriceOrder.Create('first');

  fOrder.AddItem(TOPPPriceOrderItem.Create(TOPPPriceItemDefaults.Material,1000));
  fOrder.AddItem(TOPPPriceOrderItem.Create(TOPPPriceItemDefaults.PKI,200));

  result.Add(fOrder);
end;

end.
