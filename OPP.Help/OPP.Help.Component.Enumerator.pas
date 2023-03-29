unit OPP.Help.Component.Enumerator;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls,
  OPP.Help.Meta;

type

  TOPPHelpComponentEnumerator = class helper for TComponent
  public
    function GetChildrenRecursive(): TList<TComponent>;
    function FindSubControl(Meta: TOPPHelpMeta): TControl;
  end;

implementation

function TOPPHelpComponentEnumerator.GetChildrenRecursive(): TList<TComponent>;
var
  fChildComponent: TComponent;
  i: Integer;
begin
  result := TList<TComponent>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    fChildComponent := self.Components[i];

    result.Add(fChildComponent);

    result.AddRange(fChildComponent.GetChildrenRecursive());
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
