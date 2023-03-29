program OPPHelpPreview;

uses
  Vcl.Forms,
  OPP.Help.PreviewForm in '..\OPP.Help\OPP.Help.PreviewForm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;
end.
