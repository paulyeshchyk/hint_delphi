unit OPP.Price.Item;

interface

uses
  System.Generics.Collections;

type

  TOPPPriceItem = class (TObject)
  private
    fName: String;
    fDescription: String;
    fIdentifier: String;
    function GetIdentifier: String;
  public
    constructor Create;overload;
    constructor Create(AName: String; AIdentifier: TGUID; ADescription: String);overload;
    property Identifier: String read fIdentifier;
    property Name: String read fName write fName;
    property Description: String read fDescription write fDescription;
  end;

  TOPPPriceItemList = class
  private
    fDictionary: TDictionary<String, TOPPPriceItem>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(AItem: TOPPPriceItem);
  end;

  TOPPPriceItemDefaults = class
  private
    class var fMaterial: TOPPPriceItem;
    class var fSalary: TOPPPriceItem;
    class var fPKI: TOPPPriceItem;
    class var fSellPrice: TOPPPriceItem;

    class function GetMaterial: TOPPPriceItem; static;
    class function GetSalary: TOPPPriceItem; static;
    class function theBonusSalary: TOPPPriceItem;
    class function theSellPrice: TOPPPriceItem;
    class function theCostPrice: TOPPPriceItem;
    class function theUndefined: TOPPPriceItem;
    class function theWorkshop: TOPPPriceItem;
    class function theMaterial: TOPPPriceItem;
    class function theSalary: TOPPPriceItem;
    class function thePKI: TOPPPriceItem;
    class function GetPKI: TOPPPriceItem; static;
    class function GetSellPrice: TOPPPriceItem; static;
    class function GetAll: TList<TOPPPriceItem>; static;
  public
    class property Material: TOPPPriceItem read GetMaterial;
    class property Salary: TOPPPriceItem read GetSalary;
    class property PKI: TOPPPriceItem read GetPKI;
    class property SellPrice: TOPPPriceItem read GetSellPrice;
    class property All: TList<TOPPPriceItem> read GetAll;
  end;

implementation

uses
  System.SysUtils;

{ TOPPPriceItemType }

constructor TOPPPriceItem.Create;
begin
  inherited Create;
end;

constructor TOPPPriceItem.Create(AName: String; AIdentifier: TGUID; ADescription: String);
begin
  inherited Create;

  fIdentifier := GUIDToString(AIdentifier);

  Name := AName;
  Description := ADescription;
end;

{ TOPPPriceItemTypeHelper }

class function TOPPPriceItemDefaults.GetAll: TList<TOPPPriceItem>;
begin
  result := TList<TOPPPriceItem>.Create;
  result.Add(Material);
  result.Add(Salary);
  result.Add(PKI);
  result.Add(SellPrice);
end;

class function TOPPPriceItemDefaults.GetMaterial: TOPPPriceItem;
begin
  if not Assigned(TOPPPriceItemDefaults.fMaterial) then
    TOPPPriceItemDefaults.fMaterial := theMaterial;
  result := TOPPPriceItemDefaults.fMaterial;
end;

class function TOPPPriceItemDefaults.GetPKI: TOPPPriceItem;
begin
  if not Assigned(TOPPPriceItemDefaults.fPKI) then
    TOPPPriceItemDefaults.fPKI := thePKI;
  result := TOPPPriceItemDefaults.fPKI;
end;

class function TOPPPriceItemDefaults.GetSalary: TOPPPriceItem;
begin
  if not Assigned(TOPPPriceItemDefaults.fSalary) then
    TOPPPriceItemDefaults.fSalary := theSalary;
  result := TOPPPriceItemDefaults.fSalary;
end;

class function TOPPPriceItemDefaults.GetSellPrice: TOPPPriceItem;
begin
  if not Assigned(TOPPPriceItemDefaults.fSellPrice) then
    TOPPPriceItemDefaults.fSellPrice := theSellPrice;
  result := TOPPPriceItemDefaults.fSellPrice;
end;

class function TOPPPriceItemDefaults.theBonusSalary: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('ДЗ', fGuid, 'Дополнительная заработная платапроизводственных рабочих');
end;

class function TOPPPriceItemDefaults.theCostPrice: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('ПС', fGuid, 'Полная себестоимость');
end;

class function TOPPPriceItemDefaults.theMaterial: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('СМ', fGuid, 'Сырьё и материалы');
end;

class function TOPPPriceItemDefaults.thePKI: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('ПКИ', fGuid, 'Покупные и комплектующие');
end;

class function TOPPPriceItemDefaults.theSalary: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('ОЗП', fGuid, 'Основная заработная платапроизводственных рабочих');
end;

class function TOPPPriceItemDefaults.theSellPrice: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('СЦ', fGuid, 'Свободно отпускная цена с НДС');
end;

class function TOPPPriceItemDefaults.theUndefined: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('-', fGuid, 'Прочие расходы');
end;

class function TOPPPriceItemDefaults.theWorkshop: TOPPPriceItem;
var
  fGUID: TGuid;
begin
  CreateGUID(fGuid);
  result := TOPPPriceItem.Create('ЦР', fGuid, 'Цеховые расходы');
end;

{ TOPPPriceItemList }

procedure TOPPPriceItemList.AddItem(AItem: TOPPPriceItem);
var
  fAvailableItem: TOPPPriceItem;
begin
  if not Assigned(AItem) then
    exit;
  if fDictionary.TryGetValue(AItem.Name, fAvailableItem) then
    exit;
  fDictionary.Add(AItem.Name, AItem);
end;

constructor TOPPPriceItemList.Create;
var
  fItem: TOPPPriceItem;
begin
  fDictionary := TDictionary<String, TOPPPriceItem>.Create;

  self.AddItem(TOPPPriceItemDefaults.theMaterial);
  self.AddItem(TOPPPriceItemDefaults.thePKI);
  self.AddItem(TOPPPriceItemDefaults.theSalary);
  self.AddItem(TOPPPriceItemDefaults.theBonusSalary);
  self.AddItem(TOPPPriceItemDefaults.theSellPrice);
  self.AddItem(TOPPPriceItemDefaults.theCostPrice);
  self.AddItem(TOPPPriceItemDefaults.theWorkshop);

end;

destructor TOPPPriceItemList.Destroy;
begin
  fDictionary.Clear;
  fDictionary.Free;

  inherited;
end;

function TOPPPriceItem.GetIdentifier: String;
begin
//  result := GUIDToString(fGuid);
end;

end.
