unit OPP.System;

// to disable platforms on delphi 11 follow the guide
// https://stackoverflow.com/questions/44045974/how-manage-or-clean-deploy-section-in-dproj-files

interface

type
  StringHelper = record helper for
    String
    function isEmpty(): Boolean;
    function toWideChar: PWideChar;
  end;

implementation

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
