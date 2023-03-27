unit OPP.Help.Shortcut.DatasetBind;

interface

uses
  DBClient, Data.DB,
  OPP.Help.Shortcut.Dataset;

type
  TOPPHelpShortcutDatasetBind = class
  public
    class function makeClientDataset(source: TOPPHelpShortcutDataset): TClientDataset;

  end;

implementation

class function TOPPHelpShortcutDatasetBind.makeClientDataset(source: TOPPHelpShortcutDataset): TClientDataset;
begin
  result := TClientDataset.Create(nil);
  result.FieldDefs.Add('TestField',ftString, 255);
  result.CreateDataSet;
end;

end.
