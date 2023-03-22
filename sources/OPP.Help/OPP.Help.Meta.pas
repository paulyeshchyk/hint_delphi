unit OPP.Help.Meta;

interface

uses
  OPP.Help.Nonatomic;

type
  TOPPHelpMeta = record
    propertyName: String;
    identifier: TOPPHelpMetaIdentifierType;
  public
    constructor Create(APropertyName: String; AIdentifier: TOPPHelpMetaIdentifierType);
  end;

implementation

constructor TOPPHelpMeta.Create(APropertyName: string; AIdentifier: string);
begin
  propertyName := APropertyName;
  identifier := AIdentifier;
end;

end.
