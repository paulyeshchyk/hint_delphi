unit OPP.Help.View.Helper;

interface

uses
  dxPDFViewer, dxPDFDocument,
  OPP.Help.Predicate, OPP.Help.System.Types,
  OPP.Help.System.References, OPP.Help.Log;

type
  TOPPHelpViewHelper = class helper for TdxPDFViewer
  public
    procedure RunPredicate(APredicate: TOPPHelpPredicate); overload;
    procedure RunPredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion); overload;
  end;

implementation

uses
  System.SysUtils;

const
  kEventFlowName: String = 'dxPDFViewer';

resourcestring
  SDebugStartedPredicateExecutionTemplate = 'Started predicate execution: %s';
  SDebugFinishedPredicateExecutionTemplate = 'Finished predicate execution: %s';

  { TOPPHelpViewHelper }

procedure TOPPHelpViewHelper.RunPredicate(APredicate: TOPPHelpPredicate);
var
  fCurrentPageIndex: Integer;
  fSearchResult: TdxPDFDocumentTextSearchResult;
begin

  case APredicate.keywordType of
    ktSearch:
      begin
        fCurrentPageIndex := self.CurrentPageIndex;
        fSearchResult := self.Document.FindText(APredicate.value, TdxPDFDocumentTextSearchOptions.Default, fCurrentPageIndex);
        self.CurrentPageIndex := fSearchResult.range.pageIndex;
      end;
    ktBookmark:
      begin
        //
      end;
    ktPage:
      begin
        self.CurrentPageIndex := StrToInt(APredicate.value);
      end;
    ktAny:
      begin
        //
      end;
  end;

end;

procedure TOPPHelpViewHelper.RunPredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion);
var
  nestedPredicate: TOPPHelpPredicate;
begin

  eventLogger.Flow(Format(SDebugStartedPredicateExecutionTemplate, [APredicate.asString]), kEventFlowName);

  self.RunPredicate(APredicate);

  for nestedPredicate in APredicate.predicates do
  begin
    self.RunPredicate(nestedPredicate, 1 + ALevel,
      procedure(AResult: TOPPHelpViewPredicateExecutionResult; ALevel: Integer)
      begin
      end);
  end;

  eventLogger.Flow(Format(SDebugFinishedPredicateExecutionTemplate, [APredicate.asString]), kEventFlowName);

  if assigned(completion) then
    completion(perSuccess, ALevel);
end;

end.
