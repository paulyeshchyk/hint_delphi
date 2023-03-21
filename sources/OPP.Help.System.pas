unit OPP.Help.System;

// to disable platforms on delphi 11 follow the guide
// https://stackoverflow.com/questions/44045974/how-manage-or-clean-deploy-section-in-dproj-files

interface

uses SysUtils, Windows;

type
  StringHelper = record helper for
    String
    function isEmpty(): Boolean;
    function toWideChar: PWideChar;
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

function StringHelper.toWideChar: PWideChar;
var
  oleStr: PWideChar;
begin
  GetMem(oleStr, (Length(self) + 1) * SizeOf(WideChar));
  try
    StringToWideChar(self, oleStr, Length(self) + 1);
    result := oleStr;
  finally
    FreeMem(oleStr);
  end;
end;

function StringHelper.isEmpty: Boolean;
begin
  result := (Length(self) = 0);
end;

end.
