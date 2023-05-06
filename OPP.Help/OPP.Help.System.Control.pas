unit OPP.Help.System.Control;

interface

uses Vcl.Controls, System.RTTI;

type
  TOPPControlHelper = class helper for TControl
    procedure SetTextPropertyValue(AText: String);
    function TextPropertyValue: String;
    function TextPropertyLength: Integer;
    function HasTextNonZeroLength: Boolean;
    function HasTextProp: Boolean;
    function TextSelectionLength: Integer;
    function GetCustomProp(APropName: String): TValue;
    function DLG_CODE: Cardinal;
  end;

implementation

uses System.TypInfo, System.SysUtils, WinAPI.Messages, WinAPI.Windows,
  OPP.Help.System.Str;

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
  result := Prop.GetValue(self);
end;

function TOPPControlHelper.HasTextNonZeroLength: Boolean;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := Ctx.GetType(self.ClassType).GetProperty('Text');
  result := (Prop <> nil) and (Prop.Visibility in [mvPublic, mvPublished]);
  if result then
  begin
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

procedure TOPPControlHelper.SetTextPropertyValue(AText: String);
var
  fTextBuff: PWideChar;
begin
  if not(self is TWinControl) then
    exit;

  fTextBuff := AText.ToWideChar;
  try
    try
      SendMessage(TWinControl(self).Handle, WM_SETTEXT, 0, LPARAM(fTextBuff));
    except
      on E: Exception do
      begin
        //
      end;
    end;
  finally
    FreeMem(fTextBuff);
  end;
end;

function TOPPControlHelper.TextPropertyLength: Integer;
begin
  result := 0;
  if not(self is TWinControl) then
    exit;
  result := SendMessage(TWinControl(self).Handle, WM_GETTEXTLENGTH, 0, 0);
end;

function TOPPControlHelper.TextPropertyValue: String;
var
  fText: PWideChar;
  fLength: Integer;
begin
  if not(self is TWinControl) then
  begin
    result := '';
    exit;
  end;

  fLength := self.TextPropertyLength;
  if (fLength = 0) then
  begin
    result := '';
    exit;
  end;

  GetMem(fText, fLength + 1);
  try
    try
      SendMessage(TWinControl(self).Handle, WM_GETTEXT, fLength + 1, LPARAM(fText));
      result := WideCharToString(fText);
    except
      on E: Exception do
      begin
        result := '';
      end;
    end;
  finally
    FreeMem(fText);
  end;
end;

function TOPPControlHelper.TextSelectionLength: Integer;
var
  fDWORD: DWORD;
begin
  result := -1;
  if not(self is TWinControl) then
    exit;
  fDWORD := SendMessage(TWinControl(self).Handle, EM_GETSEL, 0, 0);
  result := hiword(fDWORD) - loword(fDWORD);
end;

end.
