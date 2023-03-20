unit OPP.Help.Hint.FormHelper;

interface

uses
  System.Generics.Collections,
  Vcl.Forms, Vcl.Controls,
  dxCustomHint, dxScreenTip,

  OPP.Vcl.Controls,
  OPP.Help.Hint,
  OPP.Help.Hint.Server;

type
  TOPPHintDialogCompletion = reference to procedure(Form: TForm);

  TOPPHelpHintFormHelper = class helper for TForm
  public
    procedure loadHint(tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
    procedure addTip(Hint: TOPPHelpHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
  end;

implementation

procedure TOPPHelpHintFormHelper.loadHint(tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
begin
  helpHintServer.OnHintTextsFileNameRequest := function(): String
    begin
      result := 'docs\gulfstream_manual_rtf.rtf';
    end;

  helpHintServer.getHints(self,
      procedure(hints: TList<TOPPHelpHint>)
    var
      fHint: TOPPHelpHint;
    begin
      for fHint in hints do begin
        self.addTip(fHint, tipsRepo, hintStyle);
      end;
    end);
end;

procedure TOPPHelpHintFormHelper.addTip(Hint: TOPPHelpHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
var
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
  fControl: TControl;
begin
  fControl := OPPFindControl(Hint.meta.propertyName, Hint.meta.hintIdentifier);
  if not assigned(fControl) then
    exit;

  fScreenTip := tipsRepo.Items.add;
  fScreenTip.Header.PlainText := true;
  fScreenTip.Header.Text := 'Заголовок';

  fScreenTip.Description.PlainText := false;
  fScreenTip.Description.Text := Hint.Data.rtf;

  fScreenTip.Footer.PlainText := true;
  fScreenTip.Footer.Text := 'Подвал';

  fScreenTipLink := TdxScreenTipStyle(hintStyle).ScreenTipLinks.add;
  fScreenTipLink.ScreenTip := fScreenTip;
  fScreenTipLink.Control := fControl;
end;

end.
