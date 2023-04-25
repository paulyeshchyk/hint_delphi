unit OPP.Help.Meta;

interface

uses
  System.Generics.Collections,
  System.Classes, System.SysUtils,

  OPP.Help.System.References;

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

  TSampleOnlyHelpMetaExtractorListCompletion = reference to procedure (AList:TList<TOPPHelpMeta>);

  IOPPHelpMetaFactory = interface
    /// <summary>
    /// Возворащает список TOPPHelpMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHelpMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>

    function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    procedure GetChildrenHelpMeta(AComponent: TComponent; completion: TSampleOnlyHelpMetaExtractorListCompletion);
  end;


implementation

constructor TOPPHelpMeta.Create(APropertyName: string; AIdentifier: string);
begin
  propertyName := APropertyName;
  identifier := AIdentifier;
end;

function TOPPHelpMeta.GetIsValid(): Boolean;
begin
  result := Length(Trim(identifier)) <> 0
end;

end.
