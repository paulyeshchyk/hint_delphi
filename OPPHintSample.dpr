program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sources\sampleForm1.pas' {SampleForm},
  OPP.System in 'sources\OPP.System.pas',
  OPP.VCL.Controls in 'sources\OPP.VCL.Controls.pas',
  OPP.Hint in 'sources\OPP.Hint.pas',
  OPP.VCL.Forms in 'sources\OPP.VCL.Forms.pas',
  OPP.dxRichEdit in 'sources\OPP.dxRichEdit.pas',
  OPP.VCL.Component in 'sources\OPP.VCL.Component.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;
end.
