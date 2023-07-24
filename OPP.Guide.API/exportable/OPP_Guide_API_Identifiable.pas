unit OPP_Guide_API_Identifiable;

interface

uses
  OPP_Guide_API;

type

  TOPPGuideAPIScriptContainer = class(TInterfacedObject, IOPPGuideAPIIdentifiable, IOPPGuideAPIScriptable)
  private
    fPIdentifierFieldValue: String;
    fIdentifierFieldValue: String;
  public
    function PIdentifierFieldName: String;
    function PIdentifierFieldValue: String;
    function IdentifierFieldName: String;
    function IdentifierFieldValue: String;
    function ScriptFieldName: String;
    property TheScriptName: String read ScriptFieldName;
    property IdentifierValue: String read IdentifierFieldValue write fIdentifierFieldValue;
    property IdentifierName: String read IdentifierFieldName;
    property PIdentifierValue: String read PIdentifierFieldValue write fPIdentifierFieldValue;
    property PIdentifierName: String read PIdentifierFieldName;
  end;

implementation

{ TOPPGuideAPIScriptContainer }

function TOPPGuideAPIScriptContainer.IdentifierFieldName: String;
begin
  result := 'identifier';
end;

function TOPPGuideAPIScriptContainer.IdentifierFieldValue: String;
begin
  result := fIdentifierFieldValue;
end;

function TOPPGuideAPIScriptContainer.PIdentifierFieldName: String;
begin
  result := 'pidentifier';
end;

function TOPPGuideAPIScriptContainer.PIdentifierFieldValue: String;
begin
  result := fPIdentifierFieldValue;
end;

function TOPPGuideAPIScriptContainer.ScriptFieldName: String;
begin
  result := 'script';
end;

end.
