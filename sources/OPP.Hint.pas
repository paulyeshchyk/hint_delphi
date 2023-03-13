unit OPP.Hint;

interface

uses
  System.SysUtils, System.Generics.Collections,
  OPP.System;

type

  TOPPHintIdentifierType = String;

  TOPPHintMeta = record
    propertyName: String;
    hintIdentifier: TOPPHintIdentifierType;
  end;

  TOPPHintData = record
    text: String;
    rtf: String;
  end;

  TOPPHintIdList = TList<TOPPHintMeta>;

  TOPPHint = record
    meta: TOPPHintMeta;
    data: TOPPHintData;
  end;

  TOPPHintHelper = record helper for TOPPHintData
    function isEmpty(): Boolean;
  end;

  TOPPHintServerLoadResultType = record
    error: Exception;
  end;

implementation

function TOPPHintHelper.isEmpty(): Boolean;
begin
  result := text.isEmpty() or rtf.isEmpty();
end;

end.
