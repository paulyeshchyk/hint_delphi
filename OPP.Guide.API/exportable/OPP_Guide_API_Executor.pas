unit OPP_Guide_API_Executor;

interface
uses
  OPP_Guide_API,
  OPP_Guide_API_Dataprovider,
  OPP_Guide_API_Scripter;

type
  IOPPGuideAPIExecutor = interface
    function Compile(ADataprovider: IOPPGuideAPIDataprovider; AScripter: IOPPGuideScripter; completion: TOPPGuideAPIExecutionStateCallback): Boolean;
    function FetchAllAndRun(ADataprovider: IOPPGuideAPIDataprovider; AObject: IOPPGuideAPIIdentifiable; ADirection: TOPPGuideExecutionNodeDirection; AScripter: IOPPGuideScripter; AOnScriptConsoleLogOutput: TOPPGuideAPIExecutionStateCallback): Boolean; overload;
  end;

implementation

end.
