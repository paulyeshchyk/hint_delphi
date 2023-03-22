unit OPP.Help.System;

// to disable platforms on delphi 11 follow the guide
// https://stackoverflow.com/questions/44045974/how-manage-or-clean-deploy-section-in-dproj-files

interface

uses SysUtils, Windows,
  IdGlobal, IdHash, IdHashMessageDigest;

type
  StringHelper = record helper for
    String
    function isEmpty(): Boolean;
    function toWideChar: PWideChar;
    function hashString(): String;
  end;

  ErrorHelper = class helper for Exception
  public
    procedure Log();
  end;

implementation

procedure ErrorHelper.Log;
begin
  OutputDebugString(self.ClassName.toWideChar);
end;

function StringHelper.hashString(): String;
var
    hashMessageDigest5 : TIdHashMessageDigest5;
begin
    hashMessageDigest5 := nil;
    try
        hashMessageDigest5 := TIdHashMessageDigest5.Create;
        Result := IdGlobal.IndyLowerCase ( hashMessageDigest5.HashStringAsHex ( self ) );
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
