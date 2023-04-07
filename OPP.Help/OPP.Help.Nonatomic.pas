unit OPP.Help.Nonatomic;

interface

uses System.SysUtils;

type
  TOPPKeywordType = (ktSearch = 0, ktPage = 1, ktBookmark = 2, ktAny = 3);

  TOPPHelpValidation = reference to procedure (AValid: Boolean);
  TOPPHelpErrorCompletion = reference to procedure(error: Exception);
  TOPPHelpCompletion = reference to procedure();
  TOPPHelpHintMapIdentifier = String;
  TOPPHelpMetaIdentifierType = String;
  TOPPHelpShortcutMapIdentifier = String;

implementation

end.
