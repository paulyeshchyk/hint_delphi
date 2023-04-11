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
  TOPPOnMapsLoadedEvent = reference to procedure(AList: TList<TOPPHelpMap>; completion: TOPPHelpMapsCompletion);

  TOPPClientHintHelper = class
  private
    class procedure CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
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

  SampleOnly.Help.Meta.Factory;

resourcestring
  SWarningHintsAreNotDefinedTemplate = 'hints are not defined for %s';

var
  fMetaFactory: TOPPHelpMetaHintFactory;

class procedure TOPPClientHintHelper.LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
var
  fRequest: TOPPHelpHintMappingLoadRequest;
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  try
    fRequest := TOPPHelpHintMappingLoadRequest.Create(AForm, AFilename);

    try

      hintController.HintHidePause := -1;

      fRequest.OnGetHintFactory := fMetaFactory.GetChildrenHelpMeta;
      helpHintServer.LoadHints(fRequest,
        procedure(hints: TList<TOPPHelpHint>)
        begin
          TOPPClientHintHelper.CreateHintViews(AForm, hints, hintController, repo, completion);
        end);
    finally
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

class procedure TOPPClientHintHelper.SaveHints(AForm: TControl; AFilename: String; predicateFileName: String);
var
  fRequest: TOPPHelpHintMappingSaveRequest;
  fMetaFactory: TOPPHelpMetaHintFactory;
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
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

class procedure TOPPClientHintHelper.CreateHintView(AHint: TOPPHelpHint; AControl: TControl; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
begin
  TOPPHelpTipsFactory.AddTipsView(AHint, AControl, AHintController, ARepository);
end;

class procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPHelpCompletion);
var
  fHint: TOPPHelpHint;
  fControl: TComponent;
begin
  if not assigned(hints) or (hints.Count = 0) then
  begin
    eventLogger.Warning(Format(SWarningHintsAreNotDefinedTemplate, [AForm.classname]));
    if assigned(completion) then
      completion();
    exit;
  end;

  for fHint in hints do
  begin
    fControl := AForm.FindSubControl(fHint.Meta);
    CreateHintView(fHint, TControl(fControl), hintController, repo);
  end;

  if assigned(completion) then
    completion();
end;

end.
