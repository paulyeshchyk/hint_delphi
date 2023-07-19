unit OPP_Guide_API_Object_Converter;

interface

uses
  DBClient,
  System.Generics.Collections,

  OPP_Guide_API_Identifiable;

type
  TOPPGuideAPIIdentifiableList =  TList<IOPPGuideAPIIdentifiable>;
  IOPPGuideObjectConverter = interface(IUnknown)
    ['{CB5B833F-9658-4D6A-A25C-3548A01A1C6A}']
    function GetObjectFromDataset(ADataset: TClientDataset): IOPPGuideAPIIdentifiable;
    function GetObjectsFromDataset(ADataset: TClientDataset; const AFilter: String): TOPPGuideAPIIdentifiableList;
  end;

implementation

end.
