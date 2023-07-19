unit OPP_Guide_API_Identifiable;

interface

type
  IOPPGuideAPIIdentifiable = interface(IUnknown)
    ['{0852EEAF-AB86-4F05-92D3-8DE1BA867417}']
    function PIdentifierName: String;
    function PIdentifierValue: String;
    function IdentifierName: String;
    function IdentifierValue: String;
  end;

implementation

end.
