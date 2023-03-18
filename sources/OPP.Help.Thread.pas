unit OPP.Help.Thread;

interface

uses
  System.classes;

type

  TOPPHelpThreadJob = procedure of object;

  TOPPHelpThread = class(TThread)
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

constructor TOPPHelpThread.Create(job: TOPPHelpThreadJob; AOnFinish: TOPPHelpThreadJob);
begin
  inherited Create;
  fJob := job;
  fOnFinish := AOnFinish;
end;

procedure TOPPHelpThread.Execute;
begin
  inherited;
  if Assigned(fJob) then
  begin
    fJob;
  end;
end;

end.
