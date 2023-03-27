unit OPP.Help.Meta.Enumerator;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  OPP.Help.Meta, OPP.Help.system.Str,
  WinAPI.Windows;

type

  TOPPHelpComponentEnumerator = class helper for TComponent
  public

    /// <summary>
    /// ����������� ������ TOPPHelpMeta, ���������� ��� ������� ����������.
    ///
    /// ���� ��� TOPPHelpMeta ������ �� �������� �������� ����������, ���������� � ��������� propertyName
    ///
    /// </summary>
    /// <remarks> �������� propertyName �� ��������� ����� 'name'</remarks>
    function GetChildrenHelpMeta(): TList<TOPPHelpMeta>;
    function FindSubControl(Meta: TOPPHelpMeta): TControl;
    function GetHelpMeta: TOPPHelpMeta;
  end;

implementation

function TOPPHelpComponentEnumerator.GetHelpMeta: TOPPHelpMeta;
var
  fParent: TWinControl;
begin
  result := TOPPHelpMeta.Create('Name', self.Name);

  if TEdit = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (self as TEdit).HelpKeyword;
  end
  else if TPanel = self.ClassType then
  begin
    result.propertyName := 'Name';
    result.identifier := (self as TPanel).name;
  end else if TCheckBox = self.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (self as TCheckBox).HelpKeyword;
  end else begin
    if self is TWinControl then
    begin
      fParent := (self as TWinControl).Parent;
      if Assigned(fParent) and (fParent.className = 'TOppObjControl') then
      begin
        result.propertyName := 'TypeObject';
        result.identifier := '!����';
        TWinControl(self).ShowHint := true;
        TWinControl(self).parent.showHint := true;
      end;
    end;
  end;

end;

function TOPPHelpComponentEnumerator.GetChildrenHelpMeta(): TList<TOPPHelpMeta>;
var
  fChildComponent: TComponent;
  i: Integer;
  fChildComponentHintMeta: TOPPHelpMeta;
  fSelfClassName: String;
begin

  fSelfClassName := self.className;
  WinAPI.Windows.OutputDebugString(fSelfClassName.toWideChar);

  result := TList<TOPPHelpMeta>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    fChildComponent := self.Components[i];
    if not Assigned(fChildComponent) then
      continue;

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
    end;
    // recursion

    nextLevelChild := TWinControl(child).FindSubControl(Meta);
    if Assigned(nextLevelChild) then
    begin
      result := TControl(nextLevelChild);
      break;
    end;

  end;

end;

end.
