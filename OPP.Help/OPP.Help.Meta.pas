unit OPP.Help.Meta;

interface

uses
  System.Generics.Collections,
  System.Classes,
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

  IOPPHelpMetaFactory = interface
    /// <summary>
    /// Возворащает список TOPPHelpMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHelpMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>

    {function GetChildrenHelpMeta(): TList<TOPPHelpMeta>;}
    function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    function GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
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
