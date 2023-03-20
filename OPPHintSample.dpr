program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sources\sampleForm1.pas' {SampleForm},
  OPP.System in 'sources\OPP.System.pas',
  OPP.VCL.Controls in 'sources\OPP.VCL.Controls.pas',
  OPP.Help.Hint in 'sources\OPP.Help.Hint.pas',
  OPP.VCL.Component in 'sources\OPP.VCL.Component.pas',
  OPP.Help.LargeForm in 'sources\OPP.Help.LargeForm.pas' {OPPHelpLargeForm},
  AcroPDFLib_TLB in 'C:\Users\pavel\Documents\Embarcadero\Studio\19.0\Imports\AcroPDFLib_TLB.pas',
  OPP.System.Thread in 'sources\OPP.System.Thread.pas',
  OPP.Help.Shortcut.Mapping in 'sources\OPP.Help.Shortcut.Mapping.pas',
  OPP.Help.Hint.FormHelper in 'sources\OPP.Help.Hint.FormHelper.pas',
  OPP.Help.Hint.Server in 'sources\OPP.Help.Hint.Server.pas',
  OPP.Help.Shortcut.Server in 'sources\OPP.Help.Shortcut.Server.pas',
  OPP.Help.Shortcut.Request in 'sources\OPP.Help.Shortcut.Request.pas',
  OPP.Help.Shortcut.Dataset in 'sources\OPP.Help.Shortcut.Dataset.pas',
  OPP.System.Error in 'sources\OPP.System.Error.pas',
  OPP.Help.Shortcut.Mapping.Filereader in 'sources\OPP.Help.Shortcut.Mapping.Filereader.pas',
  OPP.Help.Hint.Reader in 'sources\OPP.Help.Hint.Reader.pas',
  OPP.Help.Hint.Mapping in 'sources\OPP.Help.Hint.Mapping.pas',
  OPP.Help in 'sources\OPP.Help.JSON',
  OPP.Help.Hint.Mapping.Filereader in 'sources\OPP.Help.Hint.Mapping.Filereader.pas',
  OPP.Help.Nonatomic in 'sources\OPP.Help.Nonatomic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;
end.
