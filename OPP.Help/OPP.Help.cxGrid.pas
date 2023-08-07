unit OPP.Help.cxGrid;

interface

uses
  cxGrid,
  cxGridDBTableView;

type
  TOPPHelpCXGridHelper = class
  public
    class procedure SizeColumnToFitGrid(AGrid:TcxGrid; ATableView: TcxGridDBTableView; AColumnIndex: Integer);static;
  end;

implementation

{ TOPPHelpCXGridHelper }

class procedure TOPPHelpCXGridHelper.SizeColumnToFitGrid(AGrid:TcxGrid; ATableView: TcxGridDBTableView; AColumnIndex: Integer);
var
  indicatorWidth: Integer;
  scrollIndicatorWidth: Integer;
  fColumn: TcxGridDBColumn;
  fColumnWidths: Integer;
  i: Integer;
begin
  if AColumnIndex >= ATableView.ColumnCount then
    exit;
  fColumnWidths := 0;
  for i := 0 to ATableView.ColumnCount - 1 do
  begin
    if i = AColumnIndex then
      continue;
    fColumn := ATableView.Columns[i];
    if (not fColumn.Visible) then
      continue;
    fColumnWidths := fColumnWidths + fColumn.Width;
  end;

  fColumn := ATableView.Columns[AColumnIndex];
  indicatorWidth := 0;
  if ATableView.OptionsView.Indicator then
    indicatorWidth := ATableView.OptionsView.indicatorWidth;
  scrollIndicatorWidth := 2;
  if ATableView.Site.VScrollBar.Visible then
    scrollIndicatorWidth := ATableView.Site.VScrollBar.Width;
  fColumn.Width := AGrid.Width - indicatorWidth - scrollIndicatorWidth - fColumnWidths;
end;

end.
