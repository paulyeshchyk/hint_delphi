unit OPP.Help.System.Str;

interface

type
  StringHelper = record helper for
    String
    function isEmpty(): Boolean;
    function toWideChar: PWideChar;
    function hashString(): String;
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

end.
