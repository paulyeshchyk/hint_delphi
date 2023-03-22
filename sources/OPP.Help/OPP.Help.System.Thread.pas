unit OPP.Help.System.Thread;

interface

uses
  System.classes;

type

  TOPPHelpThreadOnFinish = reference to procedure(AResult: Integer);
  TOPPHelpThreadJob = reference to procedure(onFinish: TOPPHelpThreadOnFinish);

  TOPPSystemThread = class(TThread)
  private
    fJob: TOPPHelpThreadJob;
    fOnFinish: TOPPHelpThreadOnFinish;
  public
    constructor Create(job: TOPPHelpThreadJob; AOnFinish: TOPPHelpThreadOnFinish);
    procedure Execute; override;
    property job: TOPPHelpThreadJob read fJob write fJob;
    property onFinish: TOPPHelpThreadOnFinish read fOnFinish write fOnFinish;
  end;

implementation

constructor TOPPSystemThread.Create(job: TOPPHelpThreadJob; AOnFinish: TOPPHelpThreadOnFinish);
begin
  inherited Create;
  fJob := job;
  fOnFinish := AOnFinish;
end;

procedure TOPPSystemThread.Execute;
begin
  inherited;

  if not assigned(fJob) then
    exit;

  fJob(onFinish);
end;

end.
