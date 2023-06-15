unit SampleFormWinControlOPPInfoExtractor;

interface

uses
  Vcl.Controls,
  OPP.Help.Component.Enumerator,
  OPP.Help.System.Control,
  OPP.Buffer.OPPInfo,
  OPP.Buffer.Manager;

type
  TWinControlOPPInfoExtractor = class(TOPPInfoExtractor)
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo; override;
    function isApplicable(Sender: TWinControl): Boolean; override;
    procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl); override;
  end;

implementation

{ TWinControlOPPInfoExtractor }

function TWinControlOPPInfoExtractor.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
begin
  result := nil;

  //если sender == nil тогда текст пришёл из внешнего источника
  //иначе sender должен быть TWinControl
  if (not Assigned(Sender)) or (Sender is TWinControl) then begin
    result := TOPPBufferOPPInfo.Create(otWinControl);
    if Sender.TextSelectionLength <> 0 then begin
      result.ControlText := Sender.TextSelectionPropertyValue;
    end else begin
      result.ControlText := Sender.TextPropertyValue;
    end;
  end;

end;

function TWinControlOPPInfoExtractor.isApplicable(Sender: TWinControl): Boolean;
begin
  result := (Sender is TWinControl);
end;

procedure TWinControlOPPInfoExtractor.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
begin
  if (not(AControl is TWinControl)) or (not Assigned(OPPInfo)) then
    exit;
  AControl.SetTextPropertyValue(AText);
end;

end.
