unit SampleOnly.Help.Meta.Extractor;

interface

uses
  System.Generics.Collections,
  System.Classes, System.SysUtils,
  OPP.Help.Meta;

type
  TSampleOnlyHelpMetaExtractor = class(TInterfacedObject, IOPPHelpMetaFactory)
  public
    function GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
    procedure GetChildrenHelpMeta(AComponent: TComponent; completion: TSampleOnlyHelpMetaExtractorListCompletion);
  end;

implementation

uses
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  cxButtons,
  OPP.Help.Component.Enumerator,
  OPP.Help.Log;

function TSampleOnlyHelpMetaExtractor.GetHintMeta(AComponent: TComponent): TOPPHelpMeta;
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
  end
  else if TPanel = AComponent.ClassType then
  begin
    result.propertyName := 'HelpKeyword';
    result.identifier := (AComponent as TPanel).HelpKeyword;
  end;

end;

procedure TSampleOnlyHelpMetaExtractor.GetChildrenHelpMeta(AComponent: TComponent; completion: TSampleOnlyHelpMetaExtractorListCompletion);
var
  list: TList<TComponent>;
  child: TComponent;
  fMeta: TOPPHelpMeta;
  errorText: String;
  result : TList<TOPPHelpMeta>;
begin
  result := TList<TOPPHelpMeta>.Create();

  list := AComponent.GetChildrenRecursive;
  try
    for child in list do
    begin
      fMeta := self.GetHintMeta(child);
      if not fMeta.isValid then begin
        errorText := Format('invalid meta received for component: %s - %s',[child.ClassName, child.Name]);
        eventLogger.Warning(errorText, 'TSampleOnlyHelpMetaExtractor');
        continue;
      end;
      result.Add(fMeta)
    end;

    if Assigned(completion) then
      completion(result);

  finally
    list.Free;
  end;
end;

end.
