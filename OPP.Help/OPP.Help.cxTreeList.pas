unit OPP.Help.cxTreeList;

interface

uses
  cxDBTL, cxTL;

type
  TOPPHelpCXDBTreeListHelper = class
  public
    class procedure SizeColumnToFitTree(ATree: TcxDBTreeList; AColumnIndex: Integer); static;
  end;

implementation

{ TOPPHelpCXGridHelper }

class procedure TOPPHelpCXDBTreeListHelper.SizeColumnToFitTree(ATree: TcxDBTreeList; AColumnIndex: Integer);
var
  indicatorWidth: Integer;
  scrollIndicatorWidth: Integer;
  fColumn: TcxTreeListColumn;
  fColumnWidths: Integer;
  i: Integer;
begin
  if AColumnIndex >= ATree.ColumnCount then
    exit;
  fColumnWidths := 0;
  for i := 0 to ATree.ColumnCount - 1 do
  begin
    if i = AColumnIndex then
      continue;
    fColumn := ATree.Columns[i];
    if (not fColumn.Visible) then
      continue;
    fColumnWidths := fColumnWidths + fColumn.Width;
  end;

  fColumn := ATree.Columns[AColumnIndex];
  indicatorWidth := 0;
  if ATree.OptionsView.Indicator then
    indicatorWidth := ATree.OptionsView.indicatorWidth;
  scrollIndicatorWidth := 2;
  fColumn.Width := ATree.Width - indicatorWidth - scrollIndicatorWidth - fColumnWidths;
end;

end.
