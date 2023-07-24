unit OPPGuideAPIContextContainer;

interface

uses
  System.Generics.Collections,
  OPP_Guide_API,
  OPP_Guide_API_Context;

type
  TOPPGuideAPIContextMap = TDictionary<String, TOPPGuideAPIExecutionState>;
  TOPPGuideAPIContextContainer = TList<IOPPGuideAPIContext>;

implementation

end.
