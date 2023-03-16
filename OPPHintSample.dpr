program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sources\sampleForm1.pas' {SampleForm},
  OPP.System in 'sources\OPP.System.pas',
  OPP.VCL.Controls in 'sources\OPP.VCL.Controls.pas',
  OPP.Help.HintMapping in 'sources\OPP.Help.HintMapping.pas',
  OPP.VCL.Component in 'sources\OPP.VCL.Component.pas',
  OPP.Help.LargeForm in 'sources\OPP.Help.LargeForm.pas' {OPPHelpLargeForm},
  AcroPDFLib_TLB in 'C:\Users\pavel\Documents\Embarcadero\Studio\19.0\Imports\AcroPDFLib_TLB.pas',
  OPP.VCL.Form.Help.Thread in 'sources\OPP.VCL.Form.Help.Thread.pas',
  OPP.Help.ShortcutMapping in 'sources\OPP.Help.ShortcutMapping.pas',
  OPP.Help.HintFormHelper in 'sources\OPP.Help.HintFormHelper.pas',
  OPP.Help.HintServer in 'sources\OPP.Help.HintServer.pas',
  OPP.Help.Shortcut.Server in 'sources\OPP.Help.Shortcut.Server.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;
end.
