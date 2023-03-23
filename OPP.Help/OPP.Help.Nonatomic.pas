unit OPP.Help.Nonatomic;

interface

type
  TOPPKeywordType = (ktAny, ktBookmark, ktSearch, ktPage);

  TOPPHelpHintMapIdentifier = String;
  TOPPHelpMetaIdentifierType = String;
  TOPPHelpShortcutMapIdentifier = String;

  TOPPHelpPredicate = class
  private
    fValue: String;
    fKeywordType: TOPPKeywordType;
    fFileName: String;
  public
    property value: String read fValue write fValue;
    property keywordType: TOPPKeywordType read fKeywordType write fKeywordType;
    property fileName: String read fFileName write fFileName;
  end;

implementation

end.
