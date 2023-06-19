program OPPHelpPreview;

{$R 'OPPHelpPreviewCustom.res' 'OPPHelpPreviewCustom.rc'}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  OPP.Help.PreviewForm in '..\OPP.PDF.View\OPP.Help.PreviewForm.pas' {OPPHelpPreviewForm},
  OPP.Help.View.Fullscreen in '..\OPP.PDF.View\OPP.Help.View.Fullscreen.pas',
  OPP.Help.View.Helper in '..\OPP.PDF.View\OPP.Help.View.Helper.pas',
  OPP.Help.View.ZoomSettings in '..\OPP.PDF.View\OPP.Help.View.ZoomSettings.pas' {OPPHelpPreviewZoomForm},
  OPP.Help.PreviewSettings in '..\OPP.PDF.View\OPP.Help.PreviewSettings.pas',
  OPP.Help.QuickJumpMenuBuilder in '..\OPP.PDF.View\OPP.Help.QuickJumpMenuBuilder.pas';

{$R *.res}

begin
  Application.Title := 'ГОЛЬФСТРИМ Помощь';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, OPPHelpPreviewForm);
  Application.Run;

end.
