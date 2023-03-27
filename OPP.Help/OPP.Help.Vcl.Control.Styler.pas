unit OPP.Help.Vcl.Control.Styler;

interface

uses
  classes, stdctrls;

type

  // https://en.delphipraxis.net/topic/6514-compiler-accepts-virtual-methods-in-class-helpers/
  TOPPStyler = class helper for TObject
  public
    procedure restyle; virtual;
  end;

  TButtonStyler = class helper
    (TOPPStyler) for TButton public procedure restyle;
    override;
  end;

  TLabelStyler = class helper
    (TOPPStyler) for TLabel public procedure restyle;
    override;
  end;

implementation

procedure TOPPStyler.restyle;
begin
  //
end;

procedure TButtonStyler.restyle;
begin
end;

procedure TLabelStyler.restyle;
begin
end;

end.
