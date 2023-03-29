program OPPHelpPreview;

uses
  Vcl.Forms,
  OPP.Help.PreviewForm;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;
end.
