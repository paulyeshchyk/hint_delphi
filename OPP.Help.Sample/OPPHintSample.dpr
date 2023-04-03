program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sampleForm1.pas' {SampleForm},
  OPP.Help.Hint.Reader in '..\OPP.Help\OPP.Help.Hint.Reader.pas',
  SampleOnly.Help.Hint.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Hint.Setup.pas',
  SampleOnly.Help.Shortcut.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Shortcut.Setup.pas',
  SampleOnly.Help.Meta.Factory in '..\OPP.Client.Helper\SampleOnly.Help.Meta.Factory.pas',
  sampleFormHelper in 'sampleFormHelper.pas',
  FormTest01 in 'Tests\FormTest01.pas' {FormTest1},
  FormTest02 in 'Tests\FormTest02.pas' {FormTest2},
  FormTest03 in 'Tests\FormTest03.pas' {FormTest3},
  FormTest04 in 'Tests\FormTest04.pas' {FormTest4};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;

end.
