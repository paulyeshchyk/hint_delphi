unit OPP.Guide.Executor;

interface

uses
  midasLib, Data.DB, Datasnap.DBClient,
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,
  WinAPI.Windows,
  OPP.Guide.Executor.Task,

  OPP_Guide_API_Context,

  OPP_Guide_API_Executor,
  OPP_Guide_API_Scripter,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Object_Converter,
  OPP_Guide_API_Dataprovider;

type

  TOPPGuideExecutor = class(TInterfacedObject, IOPPGuideAPIExecutor, IOPPGuideAPIContextListener)
  private
  class var
    fExecutor: TOPPGuideExecutor;
    fTasksList: TList<ITask>;
    procedure fetchNextAndRun();
  public
    class function shared: TOPPGuideExecutor; static;
    constructor Create;
    destructor Destroy; override;

    { --- IOPPGuideExecutor --- }
    function Compile(ADataprovider: IOPPGuideAPIDataprovider; AScripter: IOPPGuideScripter; completion: TOPPGuideAPIExecutionStateCallback): Boolean;
    function FetchAllAndRun(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPGuideAPIExecutionStateCallback): Boolean; overload;

    { --- IOPPGuideAPIContextListener --- }
    procedure PushNewExecutionState(AState: TOPPGuideAPIExecutionState);
  end;

implementation

uses
  Variants,
  Vcl.Forms,
  WinAPI.ShellAPI,

  OPP.Guide.API.Executor.RunStateHelper,
  OPP.Guide.Executor.Stream,
  OPP.Help.System.Messaging,
  OPP.Help.Log;

const
  kContext: String = 'TOPPGuideExecutor';

  { TOPPGuideExecutor }

class function TOPPGuideExecutor.shared: TOPPGuideExecutor;
begin
  if not Assigned(fExecutor) then
    fExecutor := TOPPGuideExecutor.Create();

  result := fExecutor;
end;

constructor TOPPGuideExecutor.Create;
begin
  inherited Create;
  fTasksList := TList<ITask>.Create;
end;

destructor TOPPGuideExecutor.Destroy;
begin
  fTasksList.Clear;
  fTasksList.Free;
  inherited;
end;

function TOPPGuideExecutor.Compile(ADataprovider: IOPPGuideAPIDataprovider; AScripter: IOPPGuideScripter; completion: TOPPGuideAPIExecutionStateCallback): Boolean;
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  System.Assert(Assigned(ADataprovider), 'Dataprovider is nil');
  fObject := ADataprovider.GetObjectConverter.GetObjectFromDataset(ADataprovider.GetDataset);

  ADataprovider.LoadScriptContent(fObject,
    procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable)
    var
      fResult: TOPPGuideAPIExecutionState;
    begin
      fResult := TOPPStreamHelper.CompileScript(AStream, AScripter, userInfo.IdentifierFieldValue);
      if Assigned(completion) then
        completion(fResult);
    end);
  result := true;
end;

function TOPPGuideExecutor.FetchAllAndRun(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPGuideAPIExecutionStateCallback): Boolean;
begin
  System.Assert(Assigned(ADataprovider), 'Dataprovider is nil');
  result := false;

  ADataprovider.ListOfNodes(AObject, ADirection,
    procedure(fItem: IOPPGuideAPIIdentifiable)
    var
      fTask: ITask;
      fThread: TThread;
    begin
      fThread := TThread.Current;

      fTask := TOPPGuideExecutorTask.CreateTask(ADataprovider, fItem, AScripter,
        procedure(AItem: IOPPGuideAPIIdentifiable; AState: TOPPGuideAPIExecutionState)
        begin

          TThread.Synchronize(fThread,
            procedure()
            begin
              if Assigned(AOnScriptConsoleLogOutput) then
                AOnScriptConsoleLogOutput(AState);

              case AState.value of
                rsvError:
                  begin
                    eventLogger.Error(AState.Description, kContext);
                    self.fetchNextAndRun();
                  end;
                rsvFinished:
                  begin
                    eventLogger.Flow(AState.Description, kContext);
                    self.fetchNextAndRun();
                  end;
              else
                begin
                  eventLogger.Flow(AState.Description, kContext);
                end;
              end;
            end);
        end);
      self.fTasksList.Add(fTask);
    end);

  fetchNextAndRun();
end;

procedure TOPPGuideExecutor.PushNewExecutionState(AState: TOPPGuideAPIExecutionState);
begin

  case AState.value of
    rsvError:
      begin
        eventLogger.Error(AState.Description, kContext);
        self.fetchNextAndRun();
      end;
    rsvFinished:
      begin
        eventLogger.Flow(AState.Description, kContext);
        self.fetchNextAndRun();
      end;
  end;

end;

procedure TOPPGuideExecutor.fetchNextAndRun;
var
  fTask: ITask;
begin
  if fTasksList.Count = 0 then
    exit;
  fTask := fTasksList.First;
  if Assigned(fTask) then
  begin
    fTask.Start;
  end;
  fTasksList.Remove(fTask);
end;

end.
