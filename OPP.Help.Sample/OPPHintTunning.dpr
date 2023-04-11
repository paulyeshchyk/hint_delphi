program OPPHintTunning;

uses
  Vcl.Forms,
  sampleForm1 in 'sampleForm1.pas' {SampleForm},
  OPP.Help.Hint.Reader in '..\OPP.Help\OPP.Help.Hint.Reader.pas',
  FormTest01 in 'Tests\FormTest01.pas' {FormTest1},
  FormTest02 in 'Tests\FormTest02.pas' {FormTest2},
  FormTest03 in 'Tests\FormTest03.pas' {FormTest3},
  SampleFormSaveState in 'Helpers\SampleFormSaveState.pas',
  SampleFormStubsHelper in 'Helpers\SampleFormStubsHelper.pas',
  OPP.Help.Settings.Form in 'OPP.Help.Settings.Form.pas' {OPPHelpSettingsForm},
  OPP.Help.Settings.Value.Editor in 'OPP.Help.Settings.Value.Editor.pas' {OPPHelpSettingsValueEditor},
  SampleOnly.Help.Hint.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Hint.Setup.pas',
  SampleOnly.Help.Meta.Factory in '..\OPP.Client.Helper\SampleOnly.Help.Meta.Factory.pas',
  SampleOnly.Help.Shortcut.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Shortcut.Setup.pas';

{$R *.res}

begin
  Application.Title := '��������� �������� ������� ������';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;

end.
