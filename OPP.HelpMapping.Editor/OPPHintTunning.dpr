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
  SampleOnly.Help.Hint.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Hint.Setup.pas',
  SampleOnly.Help.Meta.Extractor in '..\OPP.Client.Helper\SampleOnly.Help.Meta.Extractor.pas',
  SampleOnly.Help.Shortcut.Setup in '..\OPP.Client.Helper\SampleOnly.Help.Shortcut.Setup.pas',
  OPP.Help.Tips.Factory in '..\OPP.Client.Helper\OPP.Help.Tips.Factory.pas',
  OPP.Help.Settings.Form in 'Settings\OPP.Help.Settings.Form.pas' {OPPHelpSettingsForm},
  OPP.Help.Settings.Value.Editor in 'Settings\OPP.Help.Settings.Value.Editor.pas' {OPPHelpSettingsValueEditor};

{$R *.res}

begin
  Application.Title := 'Настройка индексов системы помощи';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;

end.
