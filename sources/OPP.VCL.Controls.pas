unit OPP.VCL.Controls;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  VCL.Controls,
  OPP.system,
  OPP.Hint;

type
  TComponentHintEnumerator = class helper for TControl
  public

    /// <summary>
    /// ����������� ������ TOPPHintMeta, ���������� ��� ������� ����������.
    ///
    /// ���� ��� TOPPHintMeta ������ �� �������� �������� ����������, ���������� � ��������� propertyName
    ///
    /// </summary>
    /// <remarks> �������� propertyName �� ��������� ����� 'name'</remarks>
    function GetControlHintsMeta(propertyName: String = 'name'): TList<TOPPHintMeta>;
    function FindFirst(propertyName: String; propertyValue: String): TControl;
  end;

implementation

function TComponentHintEnumerator.GetControlHintsMeta(propertyName: String): TList<TOPPHintMeta>;
var
  child: TComponent;
  i: Integer;
  fBookmarkIdentifier: String;
  fControlHint: TOPPHintMeta;
begin
  result := TList<TOPPHintMeta>.create();

  for i := 0 to ComponentCount - 1 do begin
    child := self.Components[i];
    if (child is TControl) and (IsPublishedProp(child, propertyName)) then begin
      fBookmarkIdentifier := String(GetPropValue(child, propertyName));
      if not fBookmarkIdentifier.isEmpty() then begin
        fControlHint.propertyName := propertyName;
        fControlHint.hintIdentifier := fBookmarkIdentifier;
        result.Add(fControlHint);
      end;
      result.AddRange(TControl(child).GetControlHintsMeta());
    end;
  end;
end;

function TComponentHintEnumerator.FindFirst(propertyName: String; propertyValue: String): TControl;
var
  i: Integer;
  child: TComponent;
  valueToCompare: String;
  found: Boolean;
begin
  found := false;
  result := nil;
  for i := 0 to ComponentCount - 1 do begin
    child := self.Components[i];
    if (child is TControl) and (IsPublishedProp(child, propertyName)) then begin
      valueToCompare := String(GetPropValue(child, propertyName));
      found := CompareStr(valueToCompare, propertyValue) = 0;
      if found then begin
        result := TControl(child);
        break;
      end;
    end;
  end;

end;

end.
