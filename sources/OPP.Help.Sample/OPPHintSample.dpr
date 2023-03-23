program OPPHintSample;





uses
  Vcl.Forms,
  sampleForm1 in 'sampleForm1.pas' {SampleForm},
  OPP.Help.Hint.FormHelper in '..\OPP.Help\OPP.Help.Hint.FormHelper.pas',
  OPP.Help.Hint.Mapping.Filereader in '..\OPP.Help\OPP.Help.Hint.Mapping.Filereader.pas',
  OPP.Help.Hint.Mapping in '..\OPP.Help\OPP.Help.Hint.Mapping.pas',
  OPP.Help.Hint in '..\OPP.Help\OPP.Help.Hint.pas',
  OPP.Help.Hint.Reader in '..\OPP.Help\OPP.Help.Hint.Reader.pas',
  OPP.Help.Hint.Server in '..\OPP.Help\OPP.Help.Hint.Server.pas',
  OPP.Help.JSON in '..\OPP.Help\OPP.Help.JSON.pas',
  OPP.Help.LargeForm in '..\OPP.Help\OPP.Help.LargeForm.pas' {OPPHelpLargeForm},
  OPP.Help.Meta.Enumerator in '..\OPP.Help\OPP.Help.Meta.Enumerator.pas',
  OPP.Help.Meta in '..\OPP.Help\OPP.Help.Meta.pas',
  OPP.Help.Nonatomic in '..\OPP.Help\OPP.Help.Nonatomic.pas',
  OPP.Help.Shortcut.Dataset in '..\OPP.Help\OPP.Help.Shortcut.Dataset.pas',
  OPP.Help.Shortcut.Mapping.Filereader in '..\OPP.Help\OPP.Help.Shortcut.Mapping.Filereader.pas',
  OPP.Help.Shortcut.Mapping in '..\OPP.Help\OPP.Help.Shortcut.Mapping.pas',
  OPP.Help.Shortcut.Request in '..\OPP.Help\OPP.Help.Shortcut.Request.pas',
  OPP.Help.Shortcut.Server in '..\OPP.Help\OPP.Help.Shortcut.Server.pas',
  OPP.Help.System.Error in '..\OPP.Help\OPP.Help.System.Error.pas',
  OPP.Help.System.Str in '..\OPP.Help\OPP.Help.System.Str.pas',
  OPP.Help.System.Thread in '..\OPP.Help\OPP.Help.System.Thread.pas',
  OPP.Help.Vcl.Control.Styler in '..\OPP.Help\OPP.Help.Vcl.Control.Styler.pas',
  OPP.Help.View.Fullscreen in '..\OPP.Help\OPP.Help.View.Fullscreen.pas',
  OPP.Help.View.Hint in '..\OPP.Help\OPP.Help.View.Hint.pas',
  OPP.Help.View in '..\OPP.Help\OPP.Help.View.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.CreateForm(TOPPHelpLargeForm, OPPHelpLargeForm);
  Application.Run;
end.
