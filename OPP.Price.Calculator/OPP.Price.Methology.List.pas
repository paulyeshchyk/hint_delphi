unit OPP.Price.Methology.List;

interface

uses
  System.Classes,
  System.Generics.Collections,
  OPP.Price.Methology;

type
  TOPPPriceMethologyList = class(TObject)
  private
    fName: String;
    fList: TList<TOPPPriceMethology>;
    fIdentificator: String;
  public
    constructor Create; overload;
    constructor Create(AName: String; AIdentifier: TGUID); overload;
    destructor Destroy; override;

    procedure AddMethology(AMethology: TOPPPriceMethology);
    procedure RemoveMethology(AID: String);
    property Identificator: String read fIdentificator write fIdentificator;
    property Name: String read fName write fName;
    property List: TList<TOPPPriceMethology> read fList write fList;
  end;

implementation

uses
  System.SysUtils;

{ TOPPPriceMethologyList }

procedure TOPPPriceMethologyList.AddMethology(AMethology: TOPPPriceMethology);
begin
  if not Assigned(AMethology) then
    exit;
  fList.Add(AMethology);
end;

constructor TOPPPriceMethologyList.Create(AName: String; AIdentifier: TGUID);
begin
  inherited Create;
  fName := AName;
  fIdentificator := GuidToString(AIdentifier);
  fList := TList<TOPPPriceMethology>.Create;
end;

constructor TOPPPriceMethologyList.Create;
begin
  inherited Create;
end;

destructor TOPPPriceMethologyList.Destroy;
begin
  fList.Clear;
  fList.Free;
  inherited;
end;

procedure TOPPPriceMethologyList.RemoveMethology(AID: String);
var
  fFound, fItem: TOPPPriceMethology;
begin
  for fItem in self.List do
  begin
    if fItem.Identificator = AID then
      fFound := fItem;
  end;

  if Assigned(fFound) then
  begin
    self.List.Remove(fFound);
  end;
end;

end.
