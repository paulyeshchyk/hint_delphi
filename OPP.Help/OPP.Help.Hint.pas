unit OPP.Help.Hint;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  OPP.Help.Meta;

type

  TOPPHelpHintData = record
    text: String;
    rtf: String;
  end;

  TOPPHintIdList = TList<TOPPHelpMeta>;

  TOPPHelpHint = record
    Meta: TOPPHelpMeta;
    data: TOPPHelpHintData;
  end;

  TOPPHelpHintHelper = record helper for TOPPHelpHintData
    function isEmpty(): Boolean;
  end;

  TOPPHelpHintServerLoadResultType = record
    error: Exception;
  end;

implementation

function TOPPHelpHintHelper.isEmpty(): Boolean;
begin
  if text.isEmpty() then begin
    result := rtf.isEmpty();
  end else begin
    result := false;
  end;
end;

end.
