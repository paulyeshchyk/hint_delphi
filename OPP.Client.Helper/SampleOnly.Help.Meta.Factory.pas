unit SampleOnly.Help.Meta.Factory;

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
  Vcl.Controls, Vcl.StdCtrls,
  cxButtons,
  OPP.Help.Component.Enumerator,
  OPP.Help.Log;

function TOPPHelpMetaHintFactory.GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
begin

  if not Assigned(AComponent) then
  begin
    eventLogger.Warning('trying to get hint meta from nil component');
    exit;
  end;

  result := TOPPHelpMeta.Create('Name', AComponent.Name);

  if TEdit = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TEdit).HelpKeyword;
  end
  else if TCheckBox = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TCheckBox).HelpKeyword;
  end
  else if TcxButton = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TcxButton).HelpKeyword;
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
    result.Add(fMeta)
  end;
end;

end.
