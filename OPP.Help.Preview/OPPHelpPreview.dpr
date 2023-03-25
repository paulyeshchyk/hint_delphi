program OPPHelpPreview;

uses
  Vcl.Forms,
  OPP.Help.PreviewForm;

{$R *.res}

var
  f: TOPPHelpPreviewForm;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPHelpPreviewForm, f);
  Application.Run;
end.
