unit SampleOnly.Help.Hint.Setup;

interface

uses
  System.Classes,
  Vcl.Controls, System.Generics.Collections, System.SysUtils,
  dxScreenTip, cxHint,
  OPP.Help.Hint, OPP.Help.Meta, OPP.Help.Map,
  OPP.Help.Nonatomic,

  OPP.Help.Hint.Server;

type
  TOPPClientHintHelperLoadCompletion = reference to procedure();
  TOPPClientHintHelper = class
  public
    class procedure LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPClientHintHelperLoadCompletion);
    class procedure SaveHints(AForm: TControl; AFilename: String; predicateFileName: String);
    class procedure AvailableMaps(completion: TOPPHelpMapsCompletion);
  private
    class procedure CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
    class function OnGetHintFactory(): IOPPHelpMetaFactory;
  end;

implementation

uses
  OPP.Help.Log,
  OPP.Help.Component.Enumerator,


  SampleOnly.Help.Meta.Factory;

var
  fMetaFactory: TOPPHelpMetaHintFactory;

class procedure TOPPClientHintHelper.LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository; completion: TOPPClientHintHelperLoadCompletion);
var
  fRequest: TOPPHelpHintMappingLoadRequest;
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  try
    fRequest := TOPPHelpHintMappingLoadRequest.Create(AForm, AFilename);

    try
      fRequest.OnGetHintFactory := function(AComponent: TComponent): TList<TOPPHelpMeta>
        begin
          result := fMetaFactory.GetChildrenHelpMeta(AComponent)
        end;

      helpHintServer.LoadHints(fRequest,
        procedure(hints: TList<TOPPHelpHint>)
        begin
          TOPPClientHintHelper.CreateHintViews(AForm, hints, hintController, repo);
          if assigned(completion) then
            completion();
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

class procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
var
  fHint: TOPPHelpHint;
  fControl: TComponent;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  if not assigned(hints) then
  begin
    eventLogger.Warning('hints are not defined');
    exit;
  end;

  eventLogger.Debug(Format('will create screentips [%d]', [hints.Count]));

  for fHint in hints do
  begin

    fControl := AForm.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;

    TControl(fControl).ShowHint := true;

    fScreenTip := repo.Items.Add;
    fScreenTip.Width := 789;

    fScreenTip.Header.PlainText := true;
    fScreenTip.Header.Text := ''; // Заголовок

    fScreenTip.Description.PlainText := false;
    fScreenTip.Description.Text := fHint.Data.rtf;

    fScreenTipLink := TdxScreenTipStyle(hintController.HintStyle).ScreenTipLinks.Add;
    fScreenTipLink.ScreenTip := fScreenTip;
    fScreenTipLink.control := TControl(fControl);

  end;
end;

class procedure TOPPClientHintHelper.AvailableMaps(completion: TOPPHelpMapsCompletion);
begin
  helpHintServer.AvailableMaps(completion);
end;

class function TOPPClientHintHelper.OnGetHintFactory(): IOPPHelpMetaFactory;
begin
  result := fMetaFactory;
end;

end.
