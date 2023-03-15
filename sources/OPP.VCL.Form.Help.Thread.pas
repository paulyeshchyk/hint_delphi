unit OPP.VCL.Form.Help.Thread;

interface

uses
  System.classes;

type

  TOPPFormHelpThreadJob = procedure of object;

  TOPPFormHelpThread = class(TThread)
  private
    fJob: TOPPFormHelpThreadJob;
    fOnFinish: TOPPFormHelpThreadJob;
  public
    constructor Create(job: TOPPFormHelpThreadJob; AOnFinish: TOPPFormHelpThreadJob);
    procedure Execute; override;
    property job: TOPPFormHelpThreadJob read fJob write fJob;
    property onFinish: TOPPFormHelpThreadJob read fOnFinish write fOnFinish;
  end;


implementation


constructor TOPPFormHelpThread.Create(job: TOPPFormHelpThreadJob; AOnFinish: TOPPFormHelpThreadJob);
begin
  inherited Create;
  fJob := job;
  fOnFinish := AOnFinish;
end;

procedure TOPPFormHelpThread.Execute;
begin
  inherited;
  if Assigned(fJob) then begin
    fJob;
  end;
end;

end.

