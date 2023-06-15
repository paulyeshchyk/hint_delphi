program OPP.BufferManager;

uses
  Vcl.Forms,
  Vcl.Controls,
  JvComponentBase,
  JvClipboardMonitor,
  OPP.Buffer.Manager,
  OPP.Buffer.Form in 'OPP.Buffer.Form.pas' {OPPBufferForm},
  SampleFormWinControlOPPInfoExtractor in '..\OPP.HelpMapping.Editor\Helpers\SampleFormWinControlOPPInfoExtractor.pas';

{$R *.res}

type
  TJVMonitorListener = class
  private
    fMonitor: TJvClipboardMonitor;
    fManager: TOPPBufferManager;
    procedure onClipboardChange(Sender: TObject);
  public
    constructor Create(monitor: TJvClipboardMonitor; Manager: TOPPBufferManager);
  end;

var
  fBufferManager: TOPPBufferManager;
  fOPPBufferForm: TOPPBufferForm;
  fJvClipboardMonitor: TJvClipboardMonitor;
  fJvMonitorListener: TJVMonitorListener;

  { TJVMonitorListener }

constructor TJVMonitorListener.Create(monitor: TJvClipboardMonitor; Manager: TOPPBufferManager);
begin
  fManager := Manager;
  fMonitor := monitor;
  if assigned(monitor) and assigned(Manager) then
  begin
    monitor.OnChange := self.onClipboardChange;
  end;
end;

procedure TJVMonitorListener.onClipboardChange(Sender: TObject);
var
  fControl: TWinControl;
begin
  fControl := nil;
  if Application.Active then
    fControl := Screen.ActiveControl;
  fBufferManager.ReadDataFromControl(fControl);
end;

begin
  fJvClipboardMonitor := TJvClipboardMonitor.Create(nil);
  fBufferManager := TOPPBufferManager.Create;
  fBufferManager.RegisterOPPInfoExtractor(TWinControlOPPInfoExtractor.Create);

  fJvMonitorListener := TJVMonitorListener.Create(fJvClipboardMonitor, fBufferManager);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOPPBufferForm, fOPPBufferForm);
  fOPPBufferForm.BufferManager := fBufferManager;

  Application.Run;
end.
