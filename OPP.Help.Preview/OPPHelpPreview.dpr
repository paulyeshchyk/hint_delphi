program OPPHelpPreview;

{$R 'OPPHelpPreviewCustom.res' 'OPPHelpPreviewCustom.rc'}

uses
  Vcl.Forms,
  OPP.Help.PreviewForm in '..\OPP.Help\OPP.Help.PreviewForm.pas',
  OPP.Help.View.Fullscreen in '..\OPP.PDF.View\OPP.Help.View.Fullscreen.pas',
  OPP.Help.View.Helper in '..\OPP.PDF.View\OPP.Help.View.Helper.pas',
  OPP.Help.View.ZoomSettings in '..\OPP.PDF.View\OPP.Help.View.ZoomSettings.pas' {OPPHelpPreviewZoomForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;
end.
