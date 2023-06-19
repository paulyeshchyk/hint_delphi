unit OPP.Help.View.Helper;

interface

uses
  dxPDFViewer, dxPDFDocument,
  Vcl.Forms,
  OPP.Help.Predicate, OPP.Help.System.Types,
  OPP.Help.System.References, OPP.Help.Log;

type
  TOPPHelpViewHelper = class helper for TdxPDFViewer
  private
    procedure RunConcretePredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion); overload;
  public
    procedure RunPredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion); overload;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Threading;

const
  kContext: String = 'TdxPDFViewer';

resourcestring
  SDebugStartedPredicateExecutionTemplate = 'Started predicate execution: %s';
  SDebugFinishedPredicateExecutionTemplate = 'Finished predicate execution: %s';

  { TOPPHelpViewHelper }

procedure TOPPHelpViewHelper.RunConcretePredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion);
var
  fCurrentPageIndex: Integer;
  fSearchResult: TdxPDFDocumentTextSearchResult;
  fCurrentThread: TThread;
begin

  fCurrentThread := TThread.CurrentThread;

  TTask.Run(
    procedure
    begin
      case APredicate.keywordType of
        ktSearch:
          begin
            fCurrentPageIndex := self.CurrentPageIndex;
{$REGION 'DevEx feature fix'}
            { the hack: devEx keeps previously searched expression in memory }
            { so, in order to repeat the search with the same expression you have to start another search with fake data, i.e. with ' 'space }
            { or, you can start searching using the the very first symbol of your expression, then, continue serarching using the the remaining characters of your expression }
            { the fix should be applied in dxPDFDocument.pas: class TdxPDFDocumentSequentialTextSearch. method InternalClear }
            { i.e: FWordIndex := -1; }
            self.Document.FindText(' ', TdxPDFDocumentTextSearchOptions.Default, fCurrentPageIndex);
{$ENDREGION}
            fSearchResult := self.Document.FindText(APredicate.value, TdxPDFDocumentTextSearchOptions.Default, fCurrentPageIndex);
            self.CurrentPageIndex := fSearchResult.range.pageIndex;
            TThread.Synchronize(fCurrentThread,
              procedure
              begin
                if assigned(completion) then
                  completion(perSuccess, ALevel);
              end);
          end;
        ktBookmark:
          begin
            TThread.Synchronize(fCurrentThread,
              procedure
              begin
                if assigned(completion) then
                  completion(perSuccess, ALevel);
              end);
          end;
        ktPage:
          begin
            self.CurrentPageIndex := StrToInt(APredicate.value);
            TThread.Synchronize(fCurrentThread,
              procedure
              begin
                if assigned(completion) then
                  completion(perSuccess, ALevel);
              end);
          end;
        ktAny:
          begin
            TThread.Synchronize(fCurrentThread,
              procedure
              begin
                if assigned(completion) then
                  completion(perSuccess, ALevel);
              end);
          end;
      end;
    end);
end;

procedure TOPPHelpViewHelper.RunPredicate(APredicate: TOPPHelpPredicate; ALevel: Integer; completion: TOPPHelpViewPredicateExecutionCompletion);
begin

  eventLogger.Flow(Format(SDebugStartedPredicateExecutionTemplate, [APredicate.asString]), kContext);

  self.RunConcretePredicate(APredicate, ALevel,
    procedure(AResult: TOPPHelpViewPredicateExecutionResult; fLevel: Integer)
    var
      nestedPredicate: TOPPHelpPredicate;
    begin
      if (not(AResult = perSuccess)) then
      begin
        eventLogger.Warning('Predicate execution failed', kContext);
      end;

      if ((not assigned(APredicate.predicates)) or (APredicate.predicates.Count = 0)) then
      begin
        eventLogger.Flow(Format(SDebugFinishedPredicateExecutionTemplate, [APredicate.asString]), kContext);
        if assigned(completion) then
          completion(perSuccess, ALevel);
        exit;
      end;

      for nestedPredicate in APredicate.predicates do
      begin
        self.RunPredicate(nestedPredicate, 1 + ALevel,
          procedure(AResult: TOPPHelpViewPredicateExecutionResult; ALevel: Integer)
          begin
            eventLogger.Flow(Format(SDebugFinishedPredicateExecutionTemplate, [APredicate.asString]), kContext);
            if assigned(completion) then
              completion(perSuccess, ALevel);
          end);
      end;
    end);
end;

end.
