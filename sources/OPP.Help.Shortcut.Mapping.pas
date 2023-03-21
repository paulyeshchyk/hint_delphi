unit OPP.Help.Shortcut.Mapping;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OPP.Help.nonatomic;

type

  TOPPHelpShortcutMap = class(TObject)
  private
    fPredicate: TOPPHelpPredicate;
    fHelpKeyword: String;
    fSearchPattern: String;
    fIdentifier: TOPPHelpShortcutMapIdentifier;
  public
    property identifier: TOPPHelpShortcutMapIdentifier read fIdentifier write fIdentifier;
    property predicate: TOPPHelpPredicate read fPredicate write fPredicate;
  end;

  TOPPHelpShortcutMapSet = class(TObject)
  private
    fList: TList<TOPPHelpShortcutMap>;
  public
    property list: TList<TOPPHelpShortcutMap> read fList write fList;

    constructor Create(AList: TList<TOPPHelpShortcutMap>);
  end;

implementation

constructor TOPPHelpShortcutMapSet.Create(AList: TList<TOPPHelpShortcutMap>);
begin
  fList := TList<TOPPHelpShortcutMap>.Create;
  fList.AddRange(AList);
end;

end.
