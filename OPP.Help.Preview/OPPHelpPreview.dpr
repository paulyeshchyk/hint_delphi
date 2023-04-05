program OPPHelpPreview;

uses
  Vcl.Forms,
  OPP.Help.PreviewForm in '..\OPP.Help\OPP.Help.PreviewForm.pas',
  OPP.Help.View.Fullscreen in '..\OPP.PDF.View\OPP.Help.View.Fullscreen.pas',
  OPP.Help.Preview.Zoom in 'OPP.Help.Preview.Zoom.pas' {OPPHelpPreviewZoomForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;
end.
