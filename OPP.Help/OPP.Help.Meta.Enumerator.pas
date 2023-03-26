unit OPP.Help.Meta.Enumerator;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls,
  OPP.Help.Meta;

type

  TOPPHelpComponentEnumerator = class helper for TComponent
  public

    /// <summary>
    /// Возворащает список TOPPHelpMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHelpMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>
    function GetChildrenHelpMeta(): TList<TOPPHelpMeta>;
    function FindSubControl(Meta: TOPPHelpMeta): TControl;
    function GetHelpMeta: TOPPHelpMeta;
  end;

implementation

function TOPPHelpComponentEnumerator.GetHelpMeta: TOPPHelpMeta;
begin
  result := TOPPHelpMeta.Create('Name', self.Name);

  if TEdit = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (self as TEdit).HelpKeyword;
  end
  else if TCheckBox = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (self as TCheckBox).HelpKeyword;
  end else begin
    // nothing to do here
  end;

end;

function TOPPHelpComponentEnumerator.GetChildrenHelpMeta(): TList<TOPPHelpMeta>;
var
  fChildComponent: TComponent;
  i: Integer;
  fChildComponentHintMeta: TOPPHelpMeta;
begin
  result := TList<TOPPHelpMeta>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    fChildComponent := self.Components[i];

    fChildComponentHintMeta := fChildComponent.GetHelpMeta();
    if fChildComponentHintMeta.isValid then
      result.Add(fChildComponentHintMeta);

    result.AddRange(TWinControl(fChildComponent).GetChildrenHelpMeta());
  end;
end;

function TOPPHelpComponentEnumerator.FindSubControl(Meta: TOPPHelpMeta): TControl;
var
  i: Integer;
  child, nextLevelChild: TComponent;
  valueToCompare: String;
  found: Boolean;
begin
  result := nil;
  for i := 0 to ComponentCount - 1 do
  begin
    child := self.Components[i];
    if not(child is TControl) then
      continue;
    if (IsPublishedProp(child, Meta.propertyName)) then
    begin
      valueToCompare := String(GetPropValue(child, Meta.propertyName));
      found := CompareStr(valueToCompare, Meta.identifier) = 0;
      if found then
      begin
        result := TControl(child);
        break;
      end;

      // recursion

      nextLevelChild := TWinControl(child).FindSubControl(Meta);
      if assigned(nextLevelChild) then
      begin
        result := TControl(nextLevelChild);
        break;
      end;

    end;
  end;

end;

end.
