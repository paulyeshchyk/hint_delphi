﻿unit OPP.Help.Hint.FormHelper;

interface

uses
  System.Generics.Collections, Vcl.Forms, Vcl.Controls, dxCustomHint,
  dxScreenTip, OPP.Vcl.Controls, OPP.Help.Hint, OPP.Help.Hint.Server;

type
  TOPPHintDialogCompletion = reference to procedure(Form: TForm);

  TOPPHelpHintFormHelper = class helper for TForm
  public
    procedure loadHint(RootControl: TControl; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
    procedure addTip(Hint: TOPPHelpHint; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
  end;

implementation

uses
  OPP.Help.Hint.Mapping, OPP.Help.Hint.Mapping.Filereader;

procedure TOPPHelpHintFormHelper.loadHint(RootControl: TControl; tipsRepo: TdxScreenTipRepository; hintStyle: TcxCustomHintStyle);
var
  fHintLoadCompletion: TOPPHelpHintLoadCompletion;
  fList: TList<TOPPHelpHintMap>;
  fMap: TOPPHelpHintMap;
begin
  fHintLoadCompletion := procedure(hints: TList<TOPPHelpHint>)
    var
      fHint: TOPPHelpHint;
    begin
      for fHint in hints do
      begin
        self.addTip(fHint, tipsRepo, hintStyle);
      end;
    end;

  helpHintServer.OnHintTextsFileNameRequest := function(): string
    begin
      //result := 'docs\gulfstream_manual_rtf.rtf';
      result := 'help\hints_matrix.json';
    end;

  helpHintServer.getHints(RootControl, fHintLoadCompletion);
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
