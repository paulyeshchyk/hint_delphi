unit HintBasics;

interface

  uses
    classes,    Vcl.Forms;

  type
    TOPPHintDialogCompletion = reference to procedure(form: TForm);

    TOPPHintUserInfo = record
      hintIdentifier: Integer;
      hintText: string;
      hintPdfFilename: String;
    end;

    WideCharHelper = record helper for String
    public
      function toWideChar: PWideChar;
    end;

implementation

  function WideCharHelper.toWideChar: PWideChar;
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

end.
