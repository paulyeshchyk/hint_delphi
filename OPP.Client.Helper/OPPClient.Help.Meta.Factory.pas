﻿unit OPPClient.Help.Meta.Factory;

interface

uses
  System.Generics.Collections,
  System.Classes,
  OPP.Help.Meta;

type
  TOPPHelpMetaMappingValue = array of string;

  TOPPHelpMetaHintFactory = class(TInterfacedObject, IOPPHelpMetaFactory)
  private
    fComponentPropertyMapping: TDictionary<String, TOPPHelpMetaMappingValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    function GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
  end;

implementation

uses
  System.SysUtils, Variants,
  OPPRTTIUtils,

  OPP.Help.Component.Enumerator,
  OPP.Help.Log;

constructor TOPPHelpMetaHintFactory.Create;
begin
  inherited Create;
  fComponentPropertyMapping := TDictionary<String, TOPPHelpMetaMappingValue>.Create();
  fComponentPropertyMapping.Add('TOppObjControl', ['TypeObject']);
  fComponentPropertyMapping.Add('TOppAttrControl', ['Attribute']);
  fComponentPropertyMapping.Add('TdxDockPanel', ['Caption']);
  fComponentPropertyMapping.Add('TcxGroupBox', ['Caption']);
end;

destructor TOPPHelpMetaHintFactory.Destroy;
begin
  fComponentPropertyMapping.Free;
  inherited Destroy;
end;

function TOPPHelpMetaHintFactory.GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
var
  fListOfValidClasses: TList<TClass>;
  fIsValidProperty, fIsValidClass: Boolean;
  fClassToTest: TClass;
  fPropertyName: String;
  fPropertyValue: String;
  fListOfValidProperties: TList<String>;
  mappedPropertyNames: TOPPHelpMetaMappingValue;
  i: Integer;
  identifier: Variant;
begin
  result.propertyName := '';
  result.identifier := '';

  if not Assigned(AComponent) then
  begin
    eventLogger.Warning('trying to get hint meta from nil component');
    exit;
  end;

  fComponentPropertyMapping.TryGetValue(AComponent.ClassName, mappedPropertyNames);
  if (Length(mappedPropertyNames) = 0) then
  begin
    exit;
  end;

  for i := 0 to Length(mappedPropertyNames) - 1 do
  begin
    identifier := OPPRTTIUtils.OPPObjectDOTPropertyValueGet(AComponent, mappedPropertyNames[i]);
    if (VarIsNull(identifier) or VarIsEmpty(identifier)) then
      continue;

    result.propertyName := mappedPropertyNames[i];
    result.identifier := identifier;
    break;
  end;

end;

function TOPPHelpMetaHintFactory.GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
var
  list: TList<TComponent>;
  child: TComponent;
  fMeta: TOPPHelpMeta;
  fFilter: TList<String>;
begin
  result := TList<TOPPHelpMeta>.Create();

  fFilter := TList<String>.Create();
  try
    list := AComponent.GetChildrenRecursive(
      function(AComponent: TComponent): Boolean
      var
        fMeta: TOPPHelpMeta;
      begin
        fMeta := self.GetHintMeta(AComponent);
        result := (fMeta.isValid and (not fFilter.Contains(fMeta.identifier)));
      end,
      procedure(AComponent: TComponent)
      var
        fMeta: TOPPHelpMeta;
      begin
        fMeta := self.GetHintMeta(AComponent);
        fFilter.Add(fMeta.identifier);
      end);

    for child in list do
    begin
      fMeta := self.GetHintMeta(child);
      result.Add(fMeta);
    end;
  finally
    fFilter.Free;
  end;
end;

end.
