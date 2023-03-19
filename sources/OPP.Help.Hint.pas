unit OPP.Help.Hint;

interface

uses
  System.SysUtils, System.Generics.Collections,
  OPP.System;

type

  TOPPHintIdentifierType = String;

  TOPPHelpHintMeta = record
    propertyName: String;
    hintIdentifier: TOPPHintIdentifierType;
  end;

  TOPPHelpHintData = record
    text: String;
    rtf: String;
  end;

  TOPPHintIdList = TList<TOPPHelpHintMeta>;

  TOPPHelpHint = record
    meta: TOPPHelpHintMeta;
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
