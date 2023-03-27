unit OPP.Help.Hint;

interface

uses
  System.SysUtils, System.Generics.Collections,
  OPP.Help.Meta;

type

  TOPPHelpHintData = record
    text: String;
    rtf: String;
  end;

  TOPPHintIdList = TList<TOPPHelpMeta>;

  TOPPHelpHint = record
    meta: TOPPHelpMeta;
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
  result := text.isEmpty() or rtf.isEmpty();
end;

end.

