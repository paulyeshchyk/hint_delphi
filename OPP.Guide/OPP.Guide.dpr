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
  OPP.Help.Log in '..\OPP.Help\OPP.Help.Log.pas',
  OPP.Stream.Observer in '..\OPP.Help\OPP.Stream.Observer.pas',
  OPP.Output.Console in '..\OPP.Help\OPP.Output.Console.pas',
  OPP.Help.VCL.PanelTrigger in '..\OPP.Help\OPP.Help.VCL.PanelTrigger.pas',
  OPP.Help.System.Application in '..\OPP.Help\OPP.Help.System.Application.pas',
  OPP.Guide.Settings in 'OPP.Guide.Settings.pas',
  OPP.Help.System.Codable in '..\OPP.Help\OPP.Help.System.Codable.pas',
  OPP.Help.Interfaces in '..\OPP.Help\OPP.Help.Interfaces.pas',
  OPP.Help.Predicate in '..\OPP.Help\OPP.Help.Predicate.pas',
  OPP.Help.System.Stream in '..\OPP.Help\OPP.Help.System.Stream.pas',
  OPP.Help.System.Types in '..\OPP.Help\OPP.Help.System.Types.pas',
  OPP.Help.Hint in '..\OPP.Help\OPP.Help.Hint.pas',
  OPP.Help.Meta in '..\OPP.Help\OPP.Help.Meta.pas',
  OPP.Help.System.References in '..\OPP.Help\OPP.Help.System.References.pas',
  OPP.Help.System.Codable.Helper in '..\OPP.Help\OPP.Help.System.Codable.Helper.pas',
  OPP.Help.System.Files in '..\OPP.Help\OPP.Help.System.Files.pas',
  OPP.Help.System.Codable.FormSizeSettings in '..\OPP.Help\OPP.Help.System.Codable.FormSizeSettings.pas',
  OPP.Help.System.AppExecutor in '..\OPP.Help\OPP.Help.System.AppExecutor.pas',
  OPP.Help.System.Messaging.Pipe in '..\OPP.Help\OPP.Help.System.Messaging.Pipe.pas';

{$R *.res}

begin
  Application.Title := TOPPGuideForm.ApplicationTitle;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPGuideForm, OPPGuideForm);
  Application.Run;
end.
