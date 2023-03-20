unit OPP.VCL.Controls;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  VCL.Controls,
  OPP.system,
  OPP.Help.Hint;

type
  TComponentHintEnumerator = class helper for TControl
  public

    /// <summary>
    /// Возворащает список TOPPHintMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHintMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>
    function GetControlHintsMeta(propertyName: String = 'HelpKeyword'): TList<TOPPHelpHintMeta>;
    function OPPFindControl(propertyName: String; propertyValue: String): TControl;
  end;

implementation

function TComponentHintEnumerator.GetControlHintsMeta(propertyName: String): TList<TOPPHelpHintMeta>;
var
  child: TComponent;
  i: Integer;
  fBookmarkIdentifier: String;
  fControlHint: TOPPHelpHintMeta;
begin
  result := TList<TOPPHelpHintMeta>.create();

  for i := 0 to ComponentCount - 1 do
  begin
    child := self.Components[i];
    if (child is TControl) and (IsPublishedProp(child, propertyName)) then
    begin
      fBookmarkIdentifier := String(GetPropValue(child, propertyName));
      if not fBookmarkIdentifier.isEmpty() then
      begin
        fControlHint.propertyName := propertyName;
        fControlHint.hintIdentifier := fBookmarkIdentifier;
        result.Add(fControlHint);
      end;
      result.AddRange(TControl(child).GetControlHintsMeta());
    end;
  end;
end;

function TComponentHintEnumerator.OPPFindControl(propertyName: String; propertyValue: String): TControl;
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
    if (IsPublishedProp(child, propertyName)) then
    begin
      valueToCompare := String(GetPropValue(child, propertyName));
      found := CompareStr(valueToCompare, propertyValue) = 0;
      if found then
      begin
        result := TControl(child);
        break;
      end;

      // recursion
      nextLevelChild := TControl(child).OPPFindControl(propertyName, propertyValue);
      if assigned(nextLevelChild) then
      begin
        result := TControl(nextLevelChild);
        break;
      end;

    end;
  end;

end;

end.
