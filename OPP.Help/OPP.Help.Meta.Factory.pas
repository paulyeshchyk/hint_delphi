unit OPP.Help.Meta.Factory;

interface

uses
  System.Generics.Collections,
  System.Classes,
  OPP.Help.Meta;

type
  IOPPHelpMetaFactory = interface
    /// <summary>
    /// Возворащает список TOPPHelpMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHelpMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>

    {function GetChildrenHelpMeta(): TList<TOPPHelpMeta>;}
  end;

  TOPPHelpMetaHintFactory = class
  public
    class function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    class function GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
  end;

implementation

uses Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  OPP.Help.Component.Enumerator, OPP.Help.Log;

class function TOPPHelpMetaHintFactory.GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
begin

  if not Assigned(AComponent) then
  begin
    eventLogger.Log('trying to get hint meta from nil component', lmWarning);
    exit;
  end;

  result := TOPPHelpMeta.Create('Name', AComponent.Name);

  if TEdit = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TEdit).HelpKeyword;
  end
  else if TCheckBox = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TCheckBox).HelpKeyword;
  end else begin
    // if self.ClassName = 'TOppObjControl' then begin
    // result.propertyName := 'TypeObject';
    // result.identifier := (AComponent as TOppObjControl).TypeObject;
    // //
    // end;
    // nothing to do here
  end;

end;

class function TOPPHelpMetaHintFactory.GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
var
  list: TList<TComponent>;
  child: TComponent;
  fMeta: TOPPHelpMeta;
begin
  result := TList<TOPPHelpMeta>.Create();

  list := AComponent.GetChildrenRecursive;
  for child in list do
  begin
    fMeta := TOPPHelpMetaHintFactory.GetHintMeta(child);
    result.Add(fMeta)
  end;
end;

end.
