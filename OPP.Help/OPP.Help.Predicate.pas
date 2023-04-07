unit OPP.Help.Predicate;

interface

uses
  System.Classes, System.Generics.Collections,
  OPP.Help.System.Stream,
  OPP.Help.Nonatomic;

type

  TOPPHelpPredicate = class(TObject)
  private
    fValue: String;
    fKeywordType: TOPPKeywordType;
    fFileName: String;
    fPredicates: TList<TOPPHelpPredicate>;
    procedure SetValue(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    function copy(): TOPPHelpPredicate;
    property value: String read fValue write SetValue;
    property keywordType: TOPPKeywordType read fKeywordType write fKeywordType;
    property filename: String read fFileName write fFileName;
    property predicates: TList<TOPPHelpPredicate> read fPredicates;
  end;

  TOPPHelpPredicateStreamHelper = class helper for TOPPHelpPredicate
    function writeToStream(AStream: TStream): Boolean;
    function readFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
  end;

implementation

uses
  System.SysUtils;

function TOPPHelpPredicateStreamHelper.writeToStream(AStream: TStream): Boolean;
var
  temp: TOPPHelpPredicate;
  cnt: Integer;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  AStream.WriteString(self.value);
  AStream.WriteInteger(Integer(self.keywordType));
  AStream.WriteString(self.filename);
  cnt := self.predicates.Count;
  AStream.WriteInteger(cnt);

  if cnt > 0 then
  begin
    for temp in self.predicates do
    begin
      temp.writeToStream(AStream);
    end;
  end;

  result := true;
end;

function TOPPHelpPredicateStreamHelper.readFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
var
  i, cnt: Integer;
  child: TOPPHelpPredicate;
begin
  result := false;
  if not assigned(AStream) then
    exit;

  if moveCursorToStart then
    AStream.Position := 0;

  self.value := AStream.ReadString;
  self.keywordType := TOPPKeywordType(AStream.ReadInteger);
  self.filename := AStream.ReadString;
  cnt := AStream.ReadInteger;
  for i := 0 to cnt - 1 do
  begin
    child := TOPPHelpPredicate.Create;
    child.readFromStream(AStream, false);
    self.predicates.Add(child);
  end;
  result := true;

end;

constructor TOPPHelpPredicate.Create;
begin
  fPredicates := TList<TOPPHelpPredicate>.Create;
end;

destructor TOPPHelpPredicate.Destroy;
begin
  fPredicates.Free;
  inherited Destroy;
end;

function TOPPHelpPredicate.copy(): TOPPHelpPredicate;
var
  fChild, fNewChild: TOPPHelpPredicate;
begin

  result := TOPPHelpPredicate.Create();

  result.value := self.value;
  result.keywordType := self.keywordType;
  result.filename := self.filename;
  for fChild in self.predicates do
  begin
    fNewChild := fChild.copy();
    result.predicates.Add(fNewChild);
  end;
end;

procedure TOPPHelpPredicate.SetValue(AValue: String);
var
  fTrimmed: String;
begin
  fTrimmed := StringReplace(AValue, #13, '', [rfReplaceAll]);
  fTrimmed := StringReplace(fTrimmed, #10, '', [rfReplaceAll]);
  fValue := fTrimmed;
end;

end.
