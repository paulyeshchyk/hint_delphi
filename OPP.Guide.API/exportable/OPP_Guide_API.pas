unit OPP_Guide_API;

interface

uses
  System.Variants,
  System.Classes,
  System.SysUtils;

type

  TOPPGuideAPIExecutionValue = (rsvIdle = 0, rsvStarted = 1, rsvProgress = 2, rsvFinished = 3, rsvError = -1);

  TOPPGuideAPIExecutionState = record
    stepIdentifier: String;
    value: TOPPGuideAPIExecutionValue;
    executionResult: String;
  end;

  TOPPGuideAPIExecutionStateCallback = TProc<TOPPGuideAPIExecutionState>;

  IOPPGuideAPIIdentifiable = interface(IUnknown)
    ['{0852EEAF-AB86-4F05-92D3-8DE1BA867417}']
    function PIdentifierFieldName: String;
    function PIdentifierFieldValue: String;
    function IdentifierFieldName: String;
    function IdentifierFieldValue: String;
  end;

  IOPPGuideAPIScriptable = interface(IUnknown)
    ['{EFD8F0B6-9676-42FF-B332-42DD7D231546}']
    function ScriptFieldName: String;
  end;

  IOPPGuideAPIExecutable = interface(IUnknown)
    ['{610F0F2E-4034-4310-9F7C-D0D0FCBF9C29}']
    procedure Execute(AStepIdentifier: String; callback: TOPPGuideAPIExecutionStateCallback);
  end;


implementation

end.
