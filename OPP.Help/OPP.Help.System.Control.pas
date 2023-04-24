unit OPP.Help.System.Control;

interface

uses Vcl.Controls;

type
  TOPPControlHelper = class helper for TControl
    function HasTextProp: Boolean;
    procedure SetTextProp(AText: String);
  end;

implementation

uses System.TypInfo, System.Rtti;

function TOPPControlHelper.HasTextProp(): Boolean;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(Self.ClassType).GetProperty('Text');
  result := (Prop <> nil) and (Prop.Visibility in [mvPublic, mvPublished]);
end;

procedure TOPPControlHelper.SetTextProp(AText: String);
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(Self.ClassType).GetProperty('Text');
  Prop.SetValue(Self, AText);
end;

end.
