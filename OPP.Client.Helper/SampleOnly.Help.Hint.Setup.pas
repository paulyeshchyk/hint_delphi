unit SampleOnly.Help.Hint.Setup;

interface

uses
  System.Classes,
  Vcl.Controls, System.Generics.Collections, System.SysUtils,
  dxScreenTip, cxHint,

  OPP.Help.System.References,
  OPP.Help.Hint, OPP.Help.Meta, OPP.Help.Map,
  OPP.Help.Hint.Server;

type
  TOPPOnMapsLoadedEvent = reference to procedure(AList: TOPPHelpMapList; completion: TOPPHelpMapsCompletion);

  TOPPClientHintHelper = class
  private
    class procedure CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
    class procedure ApplyHint(AForm: TControl; AHint: TOPPHelpHint; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
  public
    class procedure LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
    class procedure SaveHints(AForm: TControl; AFilename: String; predicateFileName: String);
    class procedure CreateHintView(AHint: TOPPHelpHint; AControl: TControl; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
  end;

implementation

uses
  OPP.Help.Log,
  OPP.Help.Tips.Factory,
  OPP.Help.Component.Enumerator,
  System.Threading,
  SampleOnly.Help.Meta.Extractor;

const
  kOPPHintHidePause: Integer = MaxInt;

resourcestring
  SWarningHintsAreNotDefinedTemplate = 'hints are not defined for %s';

var
  fMetaFactory: TSampleOnlyHelpMetaExtractor;

class procedure TOPPClientHintHelper.LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
var
  fRequest: TOPPHelpHintMappingLoadRequest;
begin

  fMetaFactory := TSampleOnlyHelpMetaExtractor.Create;
  try
    fRequest := TOPPHelpHintMappingLoadRequest.Create(AForm, AFilename);
    try
      hintController.HintHidePause := kOPPHintHidePause;
      fRequest.OnGetHintFactory := fMetaFactory.GetChildrenHelpMeta;
      helpHintServer.LoadHints(fRequest,
        procedure(hints: TList<TOPPHelpHint>)
        begin
          TOPPClientHintHelper.CreateHintViews(AForm, hints, hintController, repo, completion);
        end);
    finally
      fRequest.OnGetHintFactory := nil;
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

class procedure TOPPClientHintHelper.SaveHints(AForm: TControl; AFilename: String; predicateFileName: String);
var
  fRequest: TOPPHelpHintMappingSaveRequest;
  fMetaFactory: TSampleOnlyHelpMetaExtractor;
begin

  fMetaFactory := TSampleOnlyHelpMetaExtractor.Create;
  try
    fRequest := TOPPHelpHintMappingSaveRequest.Create(AForm, AFilename);
    try
      fRequest.DefaultPredicateFileName := predicateFileName;
      fRequest.OnGetHintFactory := function(AComponent: TComponent): TList<TOPPHelpMeta>
        begin
          result := fMetaFactory.GetChildrenHelpMeta(AComponent)
        end;

      helpHintServer.SaveHints(fRequest, false, nil);

    finally
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

class procedure TOPPClientHintHelper.ApplyHint(AForm: TControl; AHint: TOPPHelpHint; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
var
  fControl: TComponent;
begin

  fControl := AForm.FindSubControl(AHint.Meta);
  if Assigned(fControl) then
  begin
    CreateHintView(AHint, TControl(fControl), AHintController, ARepository);
  end;
end;

class procedure TOPPClientHintHelper.CreateHintView(AHint: TOPPHelpHint; AControl: TControl; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
begin
  TOPPHelpTipsFactory.AddTipsView(AHint, AControl, AHintController, ARepository);
end;

{
***
*** https://learndelphi.org/how-to-improve-gui-and-make-it-more-responsive-using-background-thread/
***
}
class procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
begin
  if not assigned(hints) or (hints.Count = 0) then
  begin
    eventLogger.Warning(Format(SWarningHintsAreNotDefinedTemplate, [AForm.classname]));
    if assigned(completion) then
      completion();
    exit;
  end;

  TTask.run(
    procedure()
    var
      fHint: TOPPHelpHint;
      fTasks: TArray<ITask>;
      i: Integer;
    begin
      i := 0;
      SetLength(fTasks, hints.count);
      for fHint in hints do
      begin
        fTasks[i] := TTask.Run(
          procedure()
          begin
            TThread.Synchronize(nil,
              procedure()
              begin
                TOPPClientHintHelper.ApplyHint(AForm, fHint, hintController, repo);
              end);
          end);
        inc(i);
      end;
      TTask.WaitForAll(fTasks);

      TThread.Synchronize(nil,
        procedure()
        begin
          if assigned(completion) then
            completion();
        end);
    end);
end;

end.
