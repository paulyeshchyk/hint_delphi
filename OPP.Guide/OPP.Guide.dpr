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
  OPP.Help.System.Str in '..\OPP.Help\OPP.Help.System.Str.pas',
  OPP.Help.System.Messaging in '..\OPP.Help\OPP.Help.System.Messaging.pas',
  OPP.Help.System.Files in '..\OPP.Help\OPP.Help.System.Files.pas',
  OPP.Help.Log in '..\OPP.Help\OPP.Help.Log.pas',
  OPP.Stream.Observer in '..\OPP.Help\OPP.Stream.Observer.pas',
  OPP.Output.Console in '..\OPP.Help\OPP.Output.Console.pas',
  OPP.Help.VCL.PanelTrigger in '..\OPP.Help\OPP.Help.VCL.PanelTrigger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPGuideForm, OPPGuideForm);
  Application.Run;
end.
