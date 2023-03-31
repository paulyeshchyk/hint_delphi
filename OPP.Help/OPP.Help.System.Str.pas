unit OPP.Help.System.Str;

interface

uses sysutils;

type
  StringHelper = record helper for
    String
    function isEmpty(): Boolean;
    function toWideChar: PWideChar;
    function hashString(): String;
    function toRTF(): String;
  end;

implementation

uses
  IdGlobal, IdHash, IdHashMessageDigest;

function StringHelper.hashString(): String;
var
  hashMessageDigest5: TIdHashMessageDigest5;
begin
  hashMessageDigest5 := nil;
  try
    hashMessageDigest5 := TIdHashMessageDigest5.Create;
    Result := IdGlobal.IndyLowerCase(hashMessageDigest5.HashStringAsHex(self));
  finally
    hashMessageDigest5.Free;
  end;
end;

function StringHelper.toWideChar: PWideChar;
var
  oleStr: PWideChar;
begin
  GetMem(oleStr, (Length(self) + 1) * SizeOf(WideChar));
  try
    StringToWideChar(self, oleStr, Length(self) + 1);
    Result := oleStr;
  finally
    FreeMem(oleStr);
  end;
end;

function StringHelper.isEmpty: Boolean;
begin
  Result := (Length(self) = 0);
end;

function StringHelper.toRTF(): String;
const
  template: String = '{\rtf1\ansi\ansicpg1251\deff0\nouicompat\deflang1049{\fonttbl{\f0\fnil\fcharset0 Calibri;}}\viewkind4\uc1\pard\sl240\slmult1\f0\fs22\lang9 %s\par}';
begin
  Result := Format(template, [self]);
end;

end.
