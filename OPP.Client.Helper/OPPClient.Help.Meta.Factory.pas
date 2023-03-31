unit OPPClient.Help.Meta.Factory;

interface

uses
  System.Generics.Collections,
  System.Classes,
  OPP.Help.Meta;

type

  TOPPHelpMetaHintFactory = class(TInterfacedObject, IOPPHelpMetaFactory)
  public
    function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    function GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Controls, Vcl.StdCtrls,
  OPPRTTIUtils,
  OPP.Help.Component.Enumerator,
  OPP.Help.Log,
  cxEdit, cxButtons;

function TOPPHelpMetaHintFactory.GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
var
  fListOfValidClasses: TList<TClass>;
  fIsValidProperty, fIsValidClass: Boolean;
  fClassToTest: TClass;
  fPropertyName: String;
  fPropertyValue: String;
  fListOfValidProperties: TList<String>;
begin

  if not Assigned(AComponent) then
  begin
    eventLogger.Log('trying to get hint meta from nil component', lmWarning);
    exit;
  end;

  fListOfValidClasses := TList<TClass>.Create;
  fListOfValidClasses.Add(TEdit);
  fListOfValidClasses.Add(TButton);
  fListOfValidClasses.Add(TcxCustomEdit);
  fListOfValidClasses.Add(TcxButton);

  fListOfValidProperties := TList<String>.Create;
  fListOfValidProperties.Add('HelpKeyword');
  fListOfValidProperties.Add('Name');


  if 'TOppObjControl' = AComponent.ClassName then
  begin
    result.propertyName := 'TypeObject';
    result.identifier := OPPRTTIUtils.OPPObjectPropertyValueGet(AComponent, 'TypeObject');
    exit;
  end;

  fIsValidClass := false;
  for fClassToTest in fListOfValidClasses do
  begin
    if AComponent is fClassToTest then
    begin
      fIsValidClass := true;
      break;
    end;
  end;

  if fIsValidClass then
  begin
    fIsValidProperty := false;
    for fPropertyName in fListOfValidProperties do
    begin
      fPropertyValue := OPPRTTIUtils.OPPObjectPropertyValueGet(AComponent, fPropertyName);
      fIsValidProperty := Length(Trim(fPropertyValue)) <> 0;
      if fIsValidProperty then
      begin
        result.propertyName := fPropertyName;
        result.identifier := fPropertyValue;
        break;
      end;
    end;
  end;

end;

function TOPPHelpMetaHintFactory.GetChildrenHelpMeta(AComponent: TComponent): TList<TOPPHelpMeta>;
var
  list: TList<TComponent>;
  child: TComponent;
  fMeta: TOPPHelpMeta;
begin
  result := TList<TOPPHelpMeta>.Create();

  list := AComponent.GetChildrenRecursive;
  for child in list do
  begin
    fMeta := self.GetHintMeta(child);
    if fMeta.isValid then
    begin
      result.Add(fMeta);
    end;
  end;
end;

end.
