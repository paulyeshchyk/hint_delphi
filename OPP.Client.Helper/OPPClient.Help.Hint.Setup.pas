unit OPPClient.Help.Hint.Setup;

interface

uses
  Vcl.Controls, System.Generics.Collections, System.SysUtils,
  dxScreenTip, cxHint,
  OPP.Help.Hint, OPP.Help.Meta;

type
  TOPPClientHintHelper = class
  public
    class procedure LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
  private
    class procedure CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
    class function OnGetHintFactory(): IOPPHelpMetaFactory;
  end;

implementation

uses
  OPP.Help.Log,
  OPP.Help.Component.Enumerator,

  OPP.Help.Hint.Server,
  OPP.Help.Meta.Factory;

var
  fMetaFactory: TOPPHelpMetaHintFactory;

class procedure TOPPClientHintHelper.LoadHints(AForm: TControl; AFilename: String; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
var
  fRequest: TOPPHelpHintMappingLoadRequest;
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;

  fRequest := TOPPHelpHintMappingLoadRequest.Create;
  try
    fRequest.mappingFileName := '.\help\mapping\hints_matrix.json';
    fRequest.control := AForm;
    fRequest.OnGetHintFactory := OnGetHintFactory;
    helpHintServer.LoadHints(fRequest,
      procedure(hints: TList<TOPPHelpHint>)
      begin
        TOPPClientHintHelper.CreateHintViews(AForm, hints, hintController, repo);
      end);
  finally
    fRequest.Free;
  end;
end;

class procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>; hintController: TcxHintStyleController; repo: TdxScreenTipRepository);
var
  fHint: TOPPHelpHint;
  fControl: TControl;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  eventLogger.Log(Format('will create screentips [%d]', [hints.Count]));

  for fHint in hints do
  begin

    fControl := AForm.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;

    fControl.ShowHint := true;

    fScreenTip := repo.Items.Add;
    fScreenTip.Width := 789;

    fScreenTip.Header.PlainText := true;
    fScreenTip.Header.Text := ''; // Заголовок

    fScreenTip.Description.PlainText := false;
    fScreenTip.Description.Text := fHint.Data.rtf;

    fScreenTipLink := TdxScreenTipStyle(hintController.HintStyle).ScreenTipLinks.Add;
    fScreenTipLink.ScreenTip := fScreenTip;
    fScreenTipLink.control := fControl;

  end;
end;

class function TOPPClientHintHelper.OnGetHintFactory(): IOPPHelpMetaFactory;
begin
  result := fMetaFactory;
end;

end.
