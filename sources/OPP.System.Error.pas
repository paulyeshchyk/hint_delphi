﻿unit OPP.System.Error;

interface

type
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
