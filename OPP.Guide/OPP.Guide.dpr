program OPP.Guide;

uses
  midaslib,
  Vcl.Forms,
  OPP.Guide.Form in 'OPP.Guide.Form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
