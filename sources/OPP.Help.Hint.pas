unit OPP.Help.Hint;

interface

uses
  System.SysUtils, System.Generics.Collections,
  OPP.Help.System;

type

  TOPPHintIdentifierType = String;

  TOPPHelpHintMeta = record
    propertyName: String;
    hintIdentifier: TOPPHintIdentifierType;
    constructor Create(APropertyName: String; AHintIdentifier: TOPPHintIdentifierType);
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

constructor TOPPHelpHintMeta.Create(APropertyName: string; AHintIdentifier: string);
begin
  propertyName := aPropertyName;
  hintIdentifier := AHintIdentifier;
end;

function TOPPHelpHintHelper.isEmpty(): Boolean;
begin
  result := text.isEmpty() or rtf.isEmpty();
end;

end.
