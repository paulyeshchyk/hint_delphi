unit OPP_Guide_API_Context_Map;

interface

uses
  OPP_Guide_API,
  System.Generics.Collections;

type
  TOPPGuideAPIContextMap = TDictionary<String, TOPPGuideExecutorRunState>;
  TOPPGuideAPIContextContainer = TList<IOPPGuideAPIContext>;

implementation

end.
