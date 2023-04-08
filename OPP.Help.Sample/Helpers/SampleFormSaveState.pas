unit SampleFormSaveState;

interface

uses
  OPP.Help.Map;

type

  THelpMapSaveCompletion = reference to procedure(ANewIdentifier: String);

  TSampleFormSaveState = class
  private
    fShortcutWasUpdated: Boolean;
    fCompletion: THelpMapSaveCompletion;
    fHintWasUpdated: Boolean;
  public
    procedure checkAndRunMap(AMap: TOPPHelpMap);
    procedure checkAndRunMapId(AIdentifier: String);

    property hintWasUpdated: Boolean read fHintWasUpdated write fHintWasUpdated;
    property shortcutWasUpdated: Boolean read fShortcutWasUpdated write fShortcutWasUpdated;
    property completion: THelpMapSaveCompletion read fCompletion write fCompletion;
  end;

implementation

procedure TSampleFormSaveState.checkAndRunMap(AMap: TOPPHelpMap);
begin
  if not Assigned(AMap) then
    exit;
  checkAndRunMapId(AMap.componentIdentifier);
end;

procedure TSampleFormSaveState.checkAndRunMapId(AIdentifier: String);
begin
  if not fShortcutWasUpdated then
    exit;
  if not fHintWasUpdated then
    exit;
  if not assigned(completion) then
    exit;

  completion(AIdentifier);
end;

end.
