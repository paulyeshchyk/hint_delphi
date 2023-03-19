unit OPP.System.Thread;

interface

uses
  System.classes;

type

  TOPPHelpThreadJob = procedure of object;

  TOPPSystemThread = class(TThread)
  private
    fJob: TOPPHelpThreadJob;
    fOnFinish: TOPPHelpThreadJob;
  public
    constructor Create(job: TOPPHelpThreadJob; AOnFinish: TOPPHelpThreadJob);
    procedure Execute; override;
    property job: TOPPHelpThreadJob read fJob write fJob;
    property onFinish: TOPPHelpThreadJob read fOnFinish write fOnFinish;
  end;

implementation

constructor TOPPSystemThread.Create(job: TOPPHelpThreadJob; AOnFinish: TOPPHelpThreadJob);
begin
  inherited Create;
  fJob := job;
  fOnFinish := AOnFinish;
end;

procedure TOPPSystemThread.Execute;
begin
  inherited;
  if Assigned(fJob) then
  begin
    fJob;
  end;
end;

end.
