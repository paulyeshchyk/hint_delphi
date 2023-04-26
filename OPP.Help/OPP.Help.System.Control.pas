unit OPP.Help.System.Control;

interface

uses Vcl.Controls, System.RTTI;

type
  TOPPControlHelper = class helper for TControl
    function HasTextNonZeroLength: Boolean;
    function HasTextProp: Boolean;
    function GetCustomProp(APropName: String): TValue;
    procedure SetTextProp(AText: String);
    function DLG_CODE: Cardinal;
  end;

implementation

uses System.TypInfo, WinAPI.Messages, WinAPI.Windows;

function TOPPControlHelper.DLG_CODE: Cardinal;
begin

  result := SendMessage(TWinControl(self).Handle, WM_GETDLGCODE, WPARAM(32), LPARAM(0)); // space character
end;

function TOPPControlHelper.GetCustomProp(APropName: String): TValue;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(self.ClassType).GetProperty(APropName);
  result := Prop.GetValue(Self);
end;

function TOPPControlHelper.HasTextNonZeroLength: Boolean;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(self.ClassType).GetProperty('Text');
  result := (Prop <> nil) and (Prop.Visibility in [mvPublic, mvPublished]);
  if result then begin
    result := Length(Prop.GetValue(self).AsString) > 0;
  end;
end;

function TOPPControlHelper.HasTextProp(): Boolean;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(self.ClassType).GetProperty('Text');
  result := (Prop <> nil) and (Prop.Visibility in [mvPublic, mvPublished]);
end;

procedure TOPPControlHelper.SetTextProp(AText: String);
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(self.ClassType).GetProperty('Text');
  Prop.SetValue(self, AText);
end;

end.
