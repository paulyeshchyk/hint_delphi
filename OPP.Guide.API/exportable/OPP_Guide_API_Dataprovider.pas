unit OPP_Guide_API_Dataprovider;

interface

uses
  System.Classes,
  Datasnap.DBClient,
  OPP_Guide_API,
  OPP_Guide_API_Object_Converter;

type

  TOPPGuideExecutionNodeDirection = (ndNodeOnly = 0, ndFromNodeToParent = 1, ndFromNodeToChildren = 2);

  TOPPGuideChainOnAddItem = reference to procedure(AItem: IOPPGuideAPIIdentifiable);

  TOPPBlobToStreamCompletion = reference to procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable);

  IOPPGuideAPIDataprovider = interface(IUnknown)
    ['{5849F28F-9DFD-4D55-A54B-085A5CD68048}']

    function GetDataset: TClientDataset;

    function GetStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function GetParentStepByIdentifier(const AIdentifier: String): IOPPGuideAPIIdentifiable;
    function AddChild(const AParentIdentifier: String): IOPPGuideAPIIdentifiable;
    function Add(): IOPPGuideAPIIdentifiable;
    function ActiveItem: IOPPGuideAPIIdentifiable;
    function ActiveItemSubscCount: Integer;
    function GetObjectConverter: IOPPGuideObjectConverter;

    procedure GetScriptedStream(AObject: IOPPGuideAPIIdentifiable; completion: TOPPBlobToStreamCompletion);
    function BuildFilter(fieldName, pident: Variant): String;
    procedure ListOfNodes(AStartFrom: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; ACompletion: TOPPGuideChainOnAddItem);
  end;

implementation

end.
