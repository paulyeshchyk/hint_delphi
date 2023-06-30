program OPPHelpPreview;

{$R 'OPPHelpPreviewCustom.res' 'OPPHelpPreviewCustom.rc'}

uses
  Vcl.Forms,
  Vcl.Dialogs,
  OPP.Help.PreviewForm in '..\OPP.PDF.View\OPP.Help.PreviewForm.pas' {OPPHelpPreviewForm},
  OPP.Help.View.Fullscreen in '..\OPP.PDF.View\OPP.Help.View.Fullscreen.pas',
  OPP.Help.View.Helper in '..\OPP.PDF.View\OPP.Help.View.Helper.pas',
  OPP.Help.View.ZoomSettings in '..\OPP.PDF.View\OPP.Help.View.ZoomSettings.pas' {OPPHelpPreviewZoomForm},
  OPP.Help.PreviewSettings in '..\OPP.PDF.View\OPP.Help.PreviewSettings.pas',
  OPP.Help.QuickJumpMenuBuilder in '..\OPP.PDF.View\OPP.Help.QuickJumpMenuBuilder.pas',
  OPP.Help.View.CommandLine in '..\OPP.PDF.View\OPP.Help.View.CommandLine.pas';

{$R *.res}

begin

  Application.Title := TOPPHelpPreviewForm.ApplicationTitle;
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;

end.
