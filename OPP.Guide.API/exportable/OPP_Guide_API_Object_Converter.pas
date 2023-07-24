unit OPP_Guide_API_Object_Converter;

interface

uses
  Datasnap.DBClient,
  System.Generics.Collections,
  OPP_Guide_API;

type
  TOPPGuideAPIIdentifiableList = TList<IOPPGuideAPIIdentifiable>;

  IOPPGuideObjectConverter = interface(IUnknown)
    ['{CB5B833F-9658-4D6A-A25C-3548A01A1C6A}']
    function GetObjectFromDataset(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
    function GetObjectsFromDataset(ADataset: TClientDataset; const AFilter: String): TOPPGuideAPIIdentifiableList;
    function NewObject(ADataset: TClientDataset): IOPPGuideAPIIdentifiable; overload;
    function NewObject(ADataset: TClientDataset; AParentIdentifier: String): IOPPGuideAPIIdentifiable; overload;
    function DescendantsCount(ADataset: TClientDataset; AParentIdentifier: String): Integer;
    function FilterForIdentifier(AIdentifier: String): String;
    function FilterForPIdentifier(AIdentifier: String): String;
  end;

  TOPPGuideAPIIdentifiableFilterType = record
    Value: String;
    class function Filter(const AFieldname: String; AFieldvalue: String): String;static;
  end;

implementation

uses
  System.SysUtils;

{ TOPPGuideAPIIdentifiableFilterType }

class function TOPPGuideAPIIdentifiableFilterType.Filter(const AFieldname: String; AFieldvalue: String): String;
begin
  result := String.Format('%s like ''%s''',[AFieldname, AFieldvalue]);
end;

end.
