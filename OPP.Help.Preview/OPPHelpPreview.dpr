program OPPHelpPreview;

uses
  Vcl.Forms,
  OPP.Help.PreviewForm in '..\OPP.Help\OPP.Help.PreviewForm.pas',
  OPP.Help.View.Fullscreen in '..\OPP.PDF.View\OPP.Help.View.Fullscreen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;
end.
