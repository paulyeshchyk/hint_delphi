unit OPP.Help.System.Error;

interface

uses SysUtils;

type

  ErrorHelper = class helper for Exception
  public
    procedure Log();
  end;

  IOPPSystemError = interface
    function errorCode(): Integer;
    function errorMessage(): String;
    function errorClass(): String;
  end;

  TOPPHelpShortcutDatasetError = class(TInterfacedObject, IOPPSystemError)
  public
    constructor Create(errorCode: Integer; errorMessage: String; errorClass: String);
    function errorCode(): Integer;
    function errorMessage(): String;
    function errorClass(): String;
  end;

implementation

uses
  WinAPI.Windows,
  OPP.Help.Log,
  OPP.Help.System.Str;

procedure ErrorHelper.Log;
var
  output: String;
begin
  output := Format('Error: %s (%s)', [self.ClassName, self.message]);
  WinAPI.Windows.OutputDebugString(output.toWideChar);
end;

constructor TOPPHelpShortcutDatasetError.Create(errorCode: Integer; errorMessage: String; errorClass: String);
begin
  inherited Create;
end;

function TOPPHelpShortcutDatasetError.errorCode(): Integer;
begin
  result := 0;
end;

function TOPPHelpShortcutDatasetError.errorMessage(): String;
begin
  result := '';
end;

function TOPPHelpShortcutDatasetError.errorClass(): String;
begin
  result := '';
end;

end.
