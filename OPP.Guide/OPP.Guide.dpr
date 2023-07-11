program OPP.Guide;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  midaslib,
  Vcl.Forms,
  OPP.Guide.Form in 'OPP.Guide.Form.pas' {OPPGuideForm},
  OPP.Guide.Executor in 'OPP.Guide.Executor.pas',
  OPP.Guide.Settings in 'OPP.Guide.Settings.pas';

{$R *.res}

begin
  Application.Title := TOPPGuideForm.ApplicationTitle;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPGuideForm, OPPGuideForm);
  Application.Run;
end.
