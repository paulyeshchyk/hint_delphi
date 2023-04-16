unit OPP.Help.System.Setting.Form.Size;

interface

uses
  Vcl.Forms, System.Types;

type
  TOPPHelpSystemSettingFormSize = class
  private
    fWindowState: TWindowState;
    fFrame: TRect;
  public
    property WindowState: TWindowState read fWindowState write fWindowState;
    property Frame: TRect read fFrame write fFrame;
  end;

implementation

end.
