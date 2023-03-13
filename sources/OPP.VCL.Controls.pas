unit OPP.VCL.Controls;

interface

uses
  system.classes, system.TypInfo, system.Generics.Collections,
  VCL.Controls,
  OPP.System,
  OPP.Hint;

type
  TComponentHintEnumerator = class helper for TControl
  public

    /// <summary>
    /// this is test summary
    /// </summary>
    /// <remarks> this is test remark</remarks>
    function GetControlHintsMeta(propertyName: String = 'name'): TList<TOPPHintMeta>;
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

end.
