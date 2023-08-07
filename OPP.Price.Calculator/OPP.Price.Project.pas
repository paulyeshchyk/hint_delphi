unit OPP.Price.Project;

interface

uses
  System.Generics.Collections,
  OPP.Price.Item,
  OPP.Price.Methology,
  OPP.Price.Methology.List;

type
  TOPPPriceProject = class
  private
    fPriceItems: TList<TOPPPriceItem>;
    fMethologyCollection: TList<TOPPPriceMethologyList>;
  public
    class function Load(AFilename: String): TOPPPriceProject; static;
    class procedure Save(AProject: TOPPPriceProject; AFilename: String); static;

    constructor Create;
    destructor Destroy; override;

    function FindMethologyList(APredicate: String):TOPPPriceMethologyList;

    property MethologyCollection: TList<TOPPPriceMethologyList> read fMethologyCollection write fMethologyCollection;
    property PriceItems: TList<TOPPPriceItem> read fPriceItems write fPriceItems;
  end;

implementation

uses
  System.JSON,
  System.IOUtils,
  REST.JSON;

{ TOPPPriceProject }

constructor TOPPPriceProject.Create;
begin
  fPriceItems := TList<TOPPPriceItem>.Create;
  fMethologyCollection := TList<TOPPPriceMethologyList>.Create;
end;

destructor TOPPPriceProject.Destroy;
begin
  fMethologyCollection.Clear;
  fMethologyCollection.Free;

  fPriceItems.Clear;
  fPriceItems.Free;
  inherited;
end;

function TOPPPriceProject.FindMethologyList(APredicate: String): TOPPPriceMethologyList;
var
  fFound: Boolean;
  fItem: TOPPPriceMethologyList;
begin
  fFound := false;
  result := nil;
  for fItem in fMethologyCollection do begin
    fFound := (fItem.Identificator = APredicate);
    if fFound then
      result := fItem;
  end;
end;

class function TOPPPriceProject.Load(AFilename: String): TOPPPriceProject;
var
  fJSON: string;
  bytes: System.TArray<System.Byte>;
  jsonObject: TJSONObject;
begin
  try
    bytes := TFile.ReadAllBytes(AFilename);

    jsonObject := TJSONObject.ParseJSONValue(bytes, 0, false) as TJSONObject;
    Result := TJson.JsonToObject<TOPPPriceProject>(jsonObject);
  except
  end;

end;

class procedure TOPPPriceProject.Save(AProject: TOPPPriceProject; AFilename: String);
var
  jsonString: String;
begin
  if (not Assigned(AProject)) then
    exit;
  jsonString := TJson.ObjectToJsonString(AProject);
  TFile.WriteAllText(AFilename, jsonString);
end;

end.
