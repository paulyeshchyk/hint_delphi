program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sources\sampleForm1.pas' {SampleForm},
  OPP.System in 'sources\OPP.System.pas',
  OPP.VCL.Controls in 'sources\OPP.VCL.Controls.pas',
  OPP.Hint in 'sources\OPP.Hint.pas',
  OPP.VCL.Forms in 'sources\OPP.VCL.Forms.pas',
  OPP.dxRichEdit in 'sources\OPP.dxRichEdit.pas',
  OPP.VCL.Component in 'sources\OPP.VCL.Component.pas',
  OPP.VCL.StdCtrls in 'sources\OPP.VCL.StdCtrls.pas',
  OPP.VCL.Form.Help in 'sources\OPP.VCL.Form.Help.pas' {OPPFormHelp},
  AcroPDFLib_TLB in 'C:\Users\pavel\Documents\Embarcadero\Studio\19.0\Imports\AcroPDFLib_TLB.pas',
  OPP.VCL.Form.Help.Thread in 'sources\OPP.VCL.Form.Help.Thread.pas',
  OPP.Help.Map in 'sources\OPP.Help.Map.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;
end.
