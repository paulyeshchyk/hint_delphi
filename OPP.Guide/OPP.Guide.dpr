program OPP.Guide;

uses
  midaslib,
  Vcl.Forms,
  OPP.Guide.Form in 'OPP.Guide.Form.pas' {OPPGuideForm},
  OPP.Guide.Executor in 'OPP.Guide.Executor.pas',
  OPP.Help.System.Str in '..\OPP.Help\OPP.Help.System.Str.pas',
  OPP.Help.System.Messaging in '..\OPP.Help\OPP.Help.System.Messaging.pas',
  OPP.Help.System.Files in '..\OPP.Help\OPP.Help.System.Files.pas',
  OPP.Help.Log in '..\OPP.Help\OPP.Help.Log.pas',
  OPP.Guide.Scripter.TMS in 'scripters\OPP.Guide.Scripter.TMS.pas',
  OPP.Guide.Scripter in 'scripters\OPP.Guide.Scripter.pas',
  OPP.Stream.Observer in '..\OPP.Help\OPP.Stream.Observer.pas',
  OPP.Output.Console in '..\OPP.Help\OPP.Output.Console.pas',
  OPP.Guide.Context in 'scripters\core\OPP.Guide.Context.pas',
  OPP.Guide.Context.TaTWrapper in 'scripters\core\OPP.Guide.Context.TaTWrapper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPGuideForm, OPPGuideForm);
  Application.Run;
end.
