unit OPP.Help.Shortcut.Mapping;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.IOUtils;

type

  TOPPHelpShortcutMap = class(TObject)
  private
    fHelpKeyword: String;
    fSearchPattern: String;
    fTestData: String;
  public
    constructor Create(AHelpKeyword: String; ASearchPattern: String);

    property TestData: String read fTestData write fTestData;
    property HelpKeyword: String read fHelpKeyword write fHelpKeyword;
    property SearchPattern: String read fSearchPattern write fSearchPattern;
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

constructor TOPPHelpShortcutMap.Create(AHelpKeyword: String; ASearchPattern: String);
begin
  inherited Create;
  fHelpKeyword := AHelpKeyword;
  fSearchPattern := ASearchPattern;
end;
end.
