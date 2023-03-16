unit OPP.Help.HintFormHelper;

interface

uses
  System.Generics.Collections,
  Vcl.Forms, Vcl.Controls,
  dxCustomHint, dxScreenTip,

  OPP.Vcl.Controls,
  OPP.Help.HintMapping,
  OPP.Help.HintServer;

type
  TOPPHintDialogCompletion = reference to procedure(Form: TForm);

  TOPPHelpHintFormHelper = class helper for TForm
  public
    procedure loadHint(tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
    procedure addTip(Hint: TOPPHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
  end;

implementation

procedure TOPPHelpHintFormHelper.loadHint(tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
var
  fHints: TList<TOPPHint>;
  fHint: TOPPHint;
begin
  fHints := helpHintServer.getHints(self);
  for fHint in fHints do begin
    self.addTip(fHint, tipsRepo, hintStyle);
  end;
end;

procedure TOPPHelpHintFormHelper.addTip(Hint: TOPPHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
var
  fTip: TdxScreenTip;
  fTipLink: TdxScreenTipLink;
  fControl: TControl;
begin
  fControl := OPPFindControl(Hint.meta.propertyName, Hint.meta.hintIdentifier);
  if not assigned(fControl) then
    exit;

  fTip := tipsRepo.Items.add;
  fTip.Header.PlainText := true;
  fTip.Header.Text := 'Заголовок';

  fTip.Description.PlainText := false;
  fTip.Description.Text := Hint.Data.rtf;

  fTip.Footer.PlainText := true;
  fTip.Footer.Text := 'Подвал';

  fTipLink := TdxScreenTipStyle(hintStyle).ScreenTipLinks.add;
  fTipLink.ScreenTip := fTip;
  fTipLink.Control := fControl;
end;

end.
