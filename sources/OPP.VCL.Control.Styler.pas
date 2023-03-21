unit OPP.VCL.Control.Styler;

interface

uses
  classes, stdctrls,
  dxBar, dxStatusBar,
  cxPC, cxGrid, cxLabel, cxCheckBox, cxRadioGroup;

type

  // https://en.delphipraxis.net/topic/6514-compiler-accepts-virtual-methods-in-class-helpers/
  TOPPStyler = class helper for TObject
  public
    procedure restyle; virtual;
  end;

  TdxBarStyler = class helper
    (TOPPStyler) for TdxBarManager public procedure restyle;
    override;
  end;

  TcxPageControlStyler = class helper
    (TOPPStyler) for TcxPageControl public procedure restyle;
    override;
  end;

  TdxStatusBarStyler = class helper
    (TOPPStyler) for TdxStatusBar public procedure restyle;
    override;
  end;

  TcxGridStyler = class helper
    (TOPPStyler) for TcxGrid public procedure restyle;
    override;
  end;

  TcxLabelStyler = class helper
    (TOPPStyler) for TcxLabel public procedure restyle;
    override;
  end;

  TcxCheckBoxStyler = class helper
    (TOPPStyler) for TcxCheckBox public procedure restyle;
    override;
  end;

  TcxRadioButtonStyler = class helper
    (TOPPStyler) for TcxRadioButton public procedure restyle;
    override;
  end;

  TLabelStyler = class helper
    (TOPPStyler) for TLabel public procedure restyle;
    override;
  end;

  TEditStyler = class helper
    (TOPPStyler) for TEdit public procedure restyle;
    override;
  end;

implementation

uses vcl.graphics;

procedure TdxBarStyler.restyle;
begin
  Style := bmsUseLookAndFeel;
  LookAndFeel.AssignedValues := []
end;

procedure TcxPageControlStyler.restyle;
begin
  LookAndFeel.AssignedValues := []
end;

procedure TdxStatusBarStyler.restyle;
begin
  PaintStyle := stpsUseLookAndFeel;
  LookAndFeel.AssignedValues := []
end;

procedure TcxGridStyler.restyle;
begin
  LookAndFeel.AssignedValues := []
end;

procedure TcxLabelStyler.restyle;
begin
  Transparent := True;
end;

procedure TcxCheckBoxStyler.restyle;
begin
  Transparent := True;
end;

procedure TcxRadioButtonStyler.restyle;
begin
  Transparent := True;
end;

procedure TLabelStyler.restyle;
begin
  Transparent := True;
end;

procedure TOPPStyler.restyle;
begin
  // nothing todo here; it is a virtual method
end;

procedure TEditStyler.restyle;
begin
end;

end.
