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
  OPP_Guide_API_Scripter,
  OPP_Guide_API,
  OPP_Guide_API_Identifiable,
  OPP_Guide_API_Object_Converter,
  OPP_Guide_API_Dataprovider;

type

  TOPPGuideExecutor = class
  private
    class var fExecutor: TOPPGuideExecutor;
  public
    class function shared: TOPPGuideExecutor; static;
    constructor Create;
    destructor Destroy; override;
    function Compile(ADataprovider: IOPPGuideAPIDataprovider; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
    function Run(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPExecutorStateCallback): Boolean; overload;
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
end;

destructor TOPPGuideExecutor.Destroy;
begin
  //
  inherited;
end;

function TOPPGuideExecutor.Compile(ADataprovider: IOPPGuideAPIDataprovider; ArunSubs: Boolean; AScripter: IOPPGuideScripter; completion: TOPPExecutorStateCallback): Boolean;
var
  fObject: IOPPGuideAPIIdentifiable;
begin
  System.Assert(Assigned(ADataprovider), 'Dataprovider is nil');
  fObject := ADataprovider.GetObjectConverter.GetObjectFromDataset(ADataprovider.GetDataset);

  ADataprovider.GetScriptedStream(fObject,
    procedure(AStream: TStream; userInfo: IOPPGuideAPIIdentifiable)
    begin
      TOPPStreamHelper.CompileScript(AStream, AScripter, userInfo.IdentifierValue, completion);
    end);
  result := true;
end;

function TOPPGuideExecutor.Run(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPExecutorStateCallback): Boolean;
begin
  result := false;

  System.Assert(Assigned(ADataprovider), 'Dataprovider is nil');

  ADataprovider.ListOfNodes(AObject, ADirection,
    procedure(fItem: IOPPGuideAPIIdentifiable)
    begin
      TOPPGuideExecutorTask.RunOnly(ADataprovider, fItem, AScripter,
        procedure(AItem: IOPPGuideAPIIdentifiable; AState: TOPPGuideExecutorRunState)
        begin
          if Assigned(AOnScriptConsoleLogOutput) then
            AOnScriptConsoleLogOutput(AState);

          case AState.value of
            rsvError:
              begin
                eventLogger.Error(AState.Description, kContext);
              end;
          else
            begin
              eventLogger.Flow(AState.Description, kContext);
            end;
          end;
        end);
    end);
end;

end.
