unit OPP.Help.Tips.Factory;

interface
uses
  Vcl.Controls,
  cxHint, dxScreenTip,
  OPP.Help.Hint;

type
  TOPPHelpTipsFactory = class
    class procedure AddTipsView(AHint: TOPPHelpHint; AControl: TControl; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
  end;

implementation

{ TOPPHelpTipsFactory }

class procedure TOPPHelpTipsFactory.AddTipsView(AHint: TOPPHelpHint; AControl: TControl; AHintController: TcxHintStyleController; ARepository: TdxScreenTipRepository);
var
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;

begin
  if not assigned(AControl) then
    exit;

  AControl.ShowHint := true;

  fScreenTip := ARepository.Items.Add;
  fScreenTip.Width := 789;

  fScreenTip.Header.PlainText := true;
  fScreenTip.Header.Text := ''; // Заголовок

  fScreenTip.Description.PlainText := false;
  fScreenTip.Description.Text := AHint.Data.rtf;

  fScreenTipLink := TdxScreenTipStyle(AHintController.HintStyle).ScreenTipLinks.Add;
  fScreenTipLink.ScreenTip := fScreenTip;
  fScreenTipLink.control := AControl;
end;

end.
