unit OPP.Help.Predicate;

interface

uses
  System.Classes,
  OPP.Help.System.Stream,
  OPP.Help.Nonatomic;

type
  TOPPHelpPredicate = class(TObject)
  private
    fValue: String;
    fKeywordType: TOPPKeywordType;
    fFileName: String;
  public
    function copy(): TOPPHelpPredicate;

    property value: String read fValue write fValue;
    property keywordType: TOPPKeywordType read fKeywordType write fKeywordType;
    property fileName: String read fFileName write fFileName;
  end;

  TOPPHelpPredicateStreamHelper = class helper for TOPPHelpPredicate
    function writeToStream(AStream: TStream): Boolean;
    function readFromStream(AStream: TStream): Boolean;
  end;

implementation

function TOPPHelpPredicate.copy(): TOPPHelpPredicate;
begin
  result := TOPPHelpPredicate.Create();
  result.value := self.value;
  result.keywordType := self.keywordType;
  result.fileName := self.fileName;
end;

function TOPPHelpPredicateStreamHelper.writeToStream(AStream: TStream): Boolean;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  AStream.WriteString(self.value);
  AStream.WriteInteger(Integer(self.keywordType));
  AStream.WriteString(self.fileName);
  result := true;
end;

function TOPPHelpPredicateStreamHelper.readFromStream(AStream: TStream): Boolean;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  self.value := AStream.ReadString;
  self.keywordType := TOPPKeywordType(AStream.ReadInteger);
  self.fileName := AStream.ReadString;
  result := true;

end;

end.
