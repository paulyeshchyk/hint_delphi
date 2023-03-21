program OPPHintSample;

uses
  Vcl.Forms,
  OPP.Help.JSON in 'sources\OPP.Help.JSON.pas',
  OPP.Help.Hint in 'sources\OPP.Help.Hint.pas',
  OPP.Help.Hint.FormHelper in 'sources\OPP.Help.Hint.FormHelper.pas',
  OPP.Help.Hint.Mapping in 'sources\OPP.Help.Hint.Mapping.pas',
  OPP.Help.Hint.Mapping.Filereader in 'sources\OPP.Help.Hint.Mapping.Filereader.pas',
  OPP.Help.Hint.Reader in 'sources\OPP.Help.Hint.Reader.pas',
  OPP.Help.Hint.Server in 'sources\OPP.Help.Hint.Server.pas',
  OPP.Help.LargeForm in 'sources\OPP.Help.LargeForm.pas' {OPPHelpLargeForm},
  OPP.Help.Nonatomic in 'sources\OPP.Help.Nonatomic.pas',
  OPP.Help.Shortcut.Dataset in 'sources\OPP.Help.Shortcut.Dataset.pas',
  OPP.Help.Shortcut.Mapping in 'sources\OPP.Help.Shortcut.Mapping.pas',
  OPP.Help.Shortcut.Mapping.Filereader in 'sources\OPP.Help.Shortcut.Mapping.Filereader.pas',
  OPP.Help.Shortcut.Request in 'sources\OPP.Help.Shortcut.Request.pas',
  OPP.Help.Shortcut.Server in 'sources\OPP.Help.Shortcut.Server.pas',
  OPP.System in 'sources\OPP.System.pas',
  OPP.System.Error in 'sources\OPP.System.Error.pas',
  OPP.System.Thread in 'sources\OPP.System.Thread.pas',
  OPP.VCL.Control.Hint in 'sources\OPP.VCL.Control.Hint.pas',
  OPP.VCL.Control.Styler in 'sources\OPP.VCL.Control.Styler.pas',
  sampleForm1 in 'sources\sampleForm1.pas' {SampleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;
end.
