program OPPHintSample;

uses
  Vcl.Forms,
  sampleForm1 in 'sampleForm1.pas' {SampleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSampleForm, SampleForm);
  Application.Run;

end.