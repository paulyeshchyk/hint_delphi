unit OPP.Help.Log.Output;

interface

uses
  System.SysUtils,
  OPP.Help.Log.Formatter;

type
  IOPPHelpLogOutput = interface
    procedure StartSession(date: TDateTime);
    procedure EndSession(date: TDateTime);
    procedure WriteText(const date: TOPPHelpLogDate; const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);
  end;

  IOPPHelpLog = interface
    procedure Custom(const AText: String; const AFlowName: String; const AFormatterClass: TOPPHelpLogFormatterClass);

    procedure SessionStart(date: TDateTime);
    procedure SessionEnd(date: TDateTime);
    procedure RegisterLogOutput(AOutput: IOPPHelpLogOutput);
  end;

implementation

end.
