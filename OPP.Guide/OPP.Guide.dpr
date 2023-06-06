program OPP.Guide;

uses
  midaslib,
  Vcl.Forms,
  OPP.Guide.Form in 'OPP.Guide.Form.pas' {Form1},
  OPP.Guide.Executor in 'OPP.Guide.Executor.pas',
  OPP.Help.System.Str in '..\OPP.Help\OPP.Help.System.Str.pas',
  OPP.Help.System.Messaging in '..\OPP.Help\OPP.Help.System.Messaging.pas',
  OPP.Help.System.Files in '..\OPP.Help\OPP.Help.System.Files.pas',
  OPP.Help.Log in '..\OPP.Help\OPP.Help.Log.pas',
  OPP.Guide.Scripter in 'OPP.Guide.Scripter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
