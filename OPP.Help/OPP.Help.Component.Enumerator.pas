unit OPP.Help.Component.Enumerator;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls,
  OPP.Help.Meta;

type

  TOPPHelpComponentComparator = reference to function(AComponent: TComponent): Boolean;

  TOPPHelpComponentEnumerator = class helper for TComponent
  public
    function GetChildrenRecursive(AComparator: TOPPHelpComponentComparator; IsControlOnly: Boolean = false): TList<TComponent>;
    function FindSubControl(Meta: TOPPHelpMeta): TComponent;
    function isSupportingMeta(Meta: TOPPHelpMeta): Boolean;
  end;

implementation

uses OPP.Help.Log;

function TOPPHelpComponentEnumerator.GetChildrenRecursive(AComparator: TOPPHelpComponentComparator; IsControlOnly: Boolean = false): TList<TComponent>;
var
  fChildComponent: TComponent;
  i: Integer;
  fComparatorResult: Boolean;
begin
  result := TList<TComponent>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    fChildComponent := self.Components[i];

    eventLogger.Flow(Format('enumerator: adding [%s]', [fChildComponent.ClassName]));

    if assigned(AComparator) then
    begin
      fComparatorResult := AComparator(fChildComponent);
      if fComparatorResult = true then
      begin
        result.Add(fChildComponent);
        result.AddRange(fChildComponent.GetChildrenRecursive(AComparator, IsControlOnly));
      end;

    end else begin
      result.Add(fChildComponent);
      result.AddRange(fChildComponent.GetChildrenRecursive(AComparator, IsControlOnly));
    end;
  end;
end;

function TOPPHelpComponentEnumerator.isSupportingMeta(Meta: TOPPHelpMeta): Boolean;
var
  valueToCompare: String;
begin
  result := false;
  if (IsPublishedProp(self, Meta.propertyName)) then
  begin
    valueToCompare := String(GetPropValue(self, Meta.propertyName));
    result := CompareStr(valueToCompare, Meta.identifier) = 0;
  end;

end;

function TOPPHelpComponentEnumerator.FindSubControl(Meta: TOPPHelpMeta): TComponent;
var
  child: TComponent;
  found: Boolean;
  fChildren: TList<TComponent>;
begin

  result := nil;

  fChildren := self.GetChildrenRecursive(
    function(AComponent: TComponent): Boolean
    begin
      result := true;
    end);
  for child in fChildren do
  begin
    eventLogger.Debug(Format('Form [%s] Iterating over component [%s]', [self.ClassName, child.ClassName]));

    if child.isSupportingMeta(Meta) then
    begin
      result := child;
      break;
    end;

  end;

end;

end.
