program OPP.Price.Calculator;

uses
  Vcl.Forms,
  OPP.Price.Calculator.Form in 'OPP.Price.Calculator.Form.pas' {OPPPriceCalculator},
  OPP.Price.Methology in 'OPP.Price.Methology.pas',
  OPP.Price.Item in 'OPP.Price.Item.pas',
  OPP.Price.Order in 'OPP.Price.Order.pas',
  OPP.Price.Methology.List in 'OPP.Price.Methology.List.pas',
  OPP.Price.Project in 'OPP.Price.Project.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPPriceCalculator, OPPPriceCalculator);
  Application.Run;
end.
