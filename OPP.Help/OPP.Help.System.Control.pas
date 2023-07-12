﻿unit OPP.Help.System.Control;

interface

uses Vcl.Controls, System.RTTI, Math;

type
  TOPPControlHelper = class helper for TControl
    procedure SetTextPropertyValue(AText: String);
    function TextPropertyValue: String;
    function TextPropertyLength: Integer;
    function HasTextNonZeroLength: Boolean;
    function HasTextProp: Boolean;
    function TextSelectionLength: Integer;
    function TextSelectionPropertyValue: String;
    function GetCustomProp(APropName: String): TValue;
    function DLG_CODE: Cardinal;
  end;

implementation

uses System.TypInfo, System.SysUtils, WinAPI.Messages, WinAPI.Windows,
  OPP.Help.System.Str, OPP.Help.Log;

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
        eventLogger.Error(E, 'TOPPControlHelper');
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
  buf: array [0 .. 254] of char;
  s: String;
begin
  if not(self is TWinControl) then
  begin
    result := '';
    exit;
  end;

  fLength := self.TextPropertyLength;
  if (fLength = 0) or (fLength > 255) then
  begin
    result := '';
    exit;
  end;

  GetWindowText(TWinControl(self).Handle, buf, 255);
  s := copy(buf, 0, fLength);
  result := s;
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

function TOPPControlHelper.TextSelectionPropertyValue: String;
var
  fText: PWideChar;
  fLength: Integer;
  fStartSelection, fEndSelection: Integer;
  buf: array [0 .. 254] of char;
  s: String;
begin
  result := '';
  if not(self is TWinControl) then
  begin
    exit;
  end;

  try
    SendMessage(TWinControl(self).Handle, EM_GETSEL, WPARAM(@(fStartSelection)), LPARAM(@(fEndSelection)));
    fLength := Math.Min((fEndSelection - fStartSelection), 254);
    if (fLength > 0) and (fLength < 255) then
    begin
      GetWindowText(TWinControl(self).Handle, buf, 254);
      s := System.copy(buf, fStartSelection, fLength);
      result := s;
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'TOPPControlHelper');
    end;
  end;
end;

end.