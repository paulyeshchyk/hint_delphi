unit OPP.Help.Predicate;

interface

uses
  System.Classes, System.Generics.Collections,
  OPP.Help.System.Stream,
  OPP.Help.System.Types,
  OPP_Help_API;

type

  TOPPHelpPredicate = class(TInterfacedObject, IOPPHelpPredicate)
  private
    fValue: String;
    fKeywordType: TOPPKeywordType;
    fFileName: String;
    fPredicates: TList<TOPPHelpPredicate>;
    procedure SetValue(AValue: String);
    function GetIsRunnable: Boolean;
    function GetValue: String;
    function GetPredicates: TList<TOPPHelpPredicate>;
    function GetKeywordType: Integer;
    procedure SetKeywordType(const Value: Integer);
    procedure SetFileName(const Value: String);
    function GetFileName: String;
  public
    constructor Create; overload;
    constructor Create(AFileName: String; AKeywordType: TOPPKeywordType; AValue: String); overload;
    destructor Destroy; override;

    property Value: String read GetValue write SetValue;
    property KeywordType: Integer read GetKeywordType write SetKeywordType;
    property Filename: String read GetFileName write SetFileName;
    property Predicates: TList<TOPPHelpPredicate> read GetPredicates;
    property IsRunnable: Boolean read GetIsRunnable;

    function WriteToStream(AStream: TStream): Boolean;
    function ReadFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
  end;

  TOPPHelpPredicateStreamHelper = class helper for TOPPHelpPredicate
    class function DefaultPredicate(): TOPPHelpPredicate;
    function asString(): String;
  end;

implementation

uses
  System.SysUtils;

function TOPPHelpPredicate.writeToStream(AStream: TStream): Boolean;
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

function TOPPHelpPredicateStreamHelper.asString: String;
begin
  result := Format('Filename: %s; Value: %s, Type:%s', [self.filename, self.value, fKeywordType.asString]);
end;

class function TOPPHelpPredicateStreamHelper.DefaultPredicate: TOPPHelpPredicate;
begin
  result := TOPPHelpPredicate.Create;
  result.filename := '.\Документация\ГОЛЬФСТРИМ_Руководство пользователя.pdf';
  result.keywordType := Integer(TOPPKeywordType.ktPage);
  result.value := '2';
end;

function TOPPHelpPredicate.readFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
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
  self.keywordType := AStream.ReadInteger;
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
  inherited Create;
  fPredicates := TList<TOPPHelpPredicate>.Create;
end;

constructor TOPPHelpPredicate.Create(AFileName: String; AKeywordType: TOPPKeywordType; AValue: String);
begin
  inherited Create;
  fPredicates := TList<TOPPHelpPredicate>.Create;
  fFileName := AFileName;
  fKeywordType := AKeywordType;
  self.value := AValue;
end;

destructor TOPPHelpPredicate.Destroy;
begin
  fFileName := '';
  fValue := '';
  fPredicates.Clear;
  FreeAndNil(fPredicates);
  inherited Destroy;
end;

function TOPPHelpPredicate.GetFileName: String;
begin
  result := fFileName;
end;

function TOPPHelpPredicate.GetIsRunnable: Boolean;
begin
  // TODO: make the same test for children
  result := false;
  if (length(fFileName) > 0) then
  begin
    result := length(fValue) > 0;
  end;
end;

function TOPPHelpPredicate.GetKeywordType: Integer;
begin
  Result := Integer(fKeywordType);
end;

function TOPPHelpPredicate.GetPredicates: TList<TOPPHelpPredicate>;
begin
  result := fPredicates;
end;

function TOPPHelpPredicate.GetValue: String;
begin
  result := fValue;
end;

procedure TOPPHelpPredicate.SetFileName(const Value: String);
begin
  fFileName := Value;
end;

procedure TOPPHelpPredicate.SetKeywordType(const Value: Integer);
begin
  fKeywordType := TOPPKeywordType(Value);
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
