unit OPP.Help.Vcl.Control.Hint;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  VCL.Controls, VCL.StdCtrls,
  OPP.Help.System,
  OPP.Help.Hint;

type

  TComponentHintEnumerator = class helper for TComponent
  public

    /// <summary>
    /// Возворащает список TOPPHintMeta, применимых для данного компонента.
    ///
    /// Ключ для TOPPHintMeta берётся из значения свойства компонента, указанного в аргументе propertyName
    ///
    /// </summary>
    /// <remarks> значение propertyName по умолчанию равно 'name'</remarks>
    function GetChildrenHintsMeta(): TList<TOPPHelpHintMeta>;
    function OPPFindControl(propertyName: String; propertyValue: String): TControl;
    function GetHintMeta: TOPPHelpHintMeta;
  end;

implementation

function TComponentHintEnumerator.GetHintMeta: TOPPHelpHintMeta;
begin
  result := TOPPHelpHintMeta.Create('Name', self.Name);

  if TEdit = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.hintIdentifier := (self as TEdit).HelpKeyword;
  end
  else if TCheckBox = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.hintIdentifier := (self as TCheckBox).HelpKeyword;
  end else begin
    // nothing to do here
  end;

end;

function TComponentHintEnumerator.GetChildrenHintsMeta(): TList<TOPPHelpHintMeta>;
var
  child: TComponent;
  i: Integer;
  fChildHintMeta: TOPPHelpHintMeta;
begin
  result := TList<TOPPHelpHintMeta>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    child := self.Components[i];

    fChildHintMeta := child.GetHintMeta();
    result.Add(fChildHintMeta);
    result.AddRange(TWinControl(child).GetChildrenHintsMeta());

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
      nextLevelChild := TWinControl(child).OPPFindControl(propertyName, propertyValue);
      if assigned(nextLevelChild) then
      begin
        result := TControl(nextLevelChild);
        break;
      end;

    end;
  end;

end;

end.
