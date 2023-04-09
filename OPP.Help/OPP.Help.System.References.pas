unit OPP.Help.System.References;

interface
uses
  System.SysUtils;
type
  TOPPHelpBooleanCompletion = reference to procedure(AValid: Boolean);
  TOPPHelpErrorCompletion = reference to procedure(error: Exception);
  TOPPHelpViewPredicateExecutionCompletion = reference to procedure(AResult: Integer);

  TOPPHelpCompletion = reference to procedure();

  TOPPHelpHintMapIdentifier = String;
  TOPPHelpMetaIdentifierType = String;
  TOPPHelpShortcutMapIdentifier = String;


implementation

end.
