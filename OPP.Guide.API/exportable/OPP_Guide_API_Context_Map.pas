unit OPP_Guide_API_Context_Map;

interface
uses
  OPP_Guide_API,
  System.Generics.Collections;

type
  TOPPGuideAPIContextMap = TDictionary<String, TOPPGuideAPIContextStepResult>;
  TOPPGuideAPIContextContainer = TList<IOPPGuideAPIContext>;

implementation

end.
