unit OPP.Help.Meta;

interface

uses
  OPP.Help.Nonatomic;

type
  TOPPHelpMeta = record
    propertyName: String;
    hintIdentifier: TOPPHintIdentifierType;
    constructor Create(APropertyName: String; AHintIdentifier: TOPPHintIdentifierType);
  end;

implementation

constructor TOPPHelpMeta.Create(APropertyName: string; AHintIdentifier: string);
begin
  propertyName := APropertyName;
  hintIdentifier := AHintIdentifier;
end;

end.
