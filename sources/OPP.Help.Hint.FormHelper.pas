﻿unit OPP.Help.Hint.FormHelper;

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
var
  fHints: TList<TOPPHelpHint>;
  fHint: TOPPHelpHint;
begin
  fHints := helpHintServer.getHints(self);
  for fHint in fHints do begin
    self.addTip(fHint, tipsRepo, hintStyle);
  end;
end;

procedure TOPPHelpHintFormHelper.addTip(Hint: TOPPHelpHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
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
