unit OPP.Price.Methology;

interface

uses
  System.Generics.Collections,
  OPP.Price.Item;

type
  TOPPPriceMethology = class (TObject)
  private
    fPriceItem: TOPPPriceItem;
    fKoefR: String;
    fKoefP: String;
    fFormula: String;
    fIdentificator: String;
  public
    constructor Create(APriceItem: TOPPPriceItem; AIdentificator: TGUID; AFormula: String; AKoefP: String = ''; AKoefR: String = '');
    property Identificator: String read fIdentificator;
    property Formula: String read fFormula write fFormula;
    property KoefP: String read fKoefP write fKoefP;
    property KoefR: String read fKoefR write fKoefR;
    property PriceItem: TOPPPriceItem read fPriceItem write fPriceItem;
  end;

implementation

uses
  System.SysUtils;

{ TOPPPriceMethology }

constructor TOPPPriceMethology.Create(APriceItem: TOPPPriceItem; AIdentificator: TGUID; AFormula: String; AKoefP: String = ''; AKoefR: String = '');
begin
  fPriceItem := APriceItem;
  fFormula := AFormula;
  fKoefP := AKoefP;
  fKoefR := AKoefR;
  fIdentificator := GUIDToString(AIdentificator);
end;

end.
