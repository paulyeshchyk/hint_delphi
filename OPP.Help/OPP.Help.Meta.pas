unit OPP.Help.Meta;

interface

uses
  OPP.Help.Nonatomic;

type
  TOPPHelpMeta = record
    propertyName: String;
    identifier: TOPPHelpMetaIdentifierType;
  private
    function GetIsValid(): Boolean;
  public
    constructor Create(APropertyName: String; AIdentifier: TOPPHelpMetaIdentifierType);
    property isValid: Boolean read GetIsValid;
  end;

implementation

constructor TOPPHelpMeta.Create(APropertyName: string; AIdentifier: string);
begin
  propertyName := APropertyName;
  identifier := AIdentifier;
end;

function TOPPHelpMeta.GetIsValid(): Boolean;
begin
  result := Length(identifier) <> 0
end;

end.
