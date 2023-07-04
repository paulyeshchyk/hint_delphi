unit OPP.Guide.Context.TaTWrapper;

interface

uses
  atscript;

type
  TOPPGuideCoreLibrary = class(TatScripterLibrary)
  protected
    procedure Init;override;
    procedure TOPPGuideContextTest(AMachine: TaTVirtualMachine);
  end;

implementation

uses
  OPP.Guide.Context;

{ TOPPGuideCoreLibrary }

procedure TOPPGuideCoreLibrary.Init;
begin
  With Scripter.AddDelphiClass(TOPPGuideContext) do begin
    DefineMethod('test', 0, tkNone, nil, TOPPGuideContextTest);
  end;

end;

procedure TOPPGuideCoreLibrary.TOPPGuideContextTest(AMachine: TaTVirtualMachine);
begin
    TOPPGuideContext(AMachine.CurrentObject).test;
end;

end.
