{
  ********
  *** source: https://github.com/malcolmgroves/delphi-experiments
  ********
}

unit OPP.Help.System.TList.Filter;

interface

uses
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TFilterFunction<T> = reference to function(Item: T): boolean;

  TFilteredList<T> = class(TList<T>)
  private
    FFilterFunction: TFilterFunction<T>;
  public

    type
    TFilteredEnumerator = class(TEnumerator<T>)
    private
      FList: TFilteredList<T>;
      FIndex: Integer;
      FFilterFunction: TFilterFunction<T>;
      function GetCurrent: T;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
      function IsLast: Boolean;
      function IsEOL: Boolean;
      function ShouldIncludeItem: Boolean;
    public
      constructor Create(AList: TFilteredList<T>; AFilterFunction: TFilterFunction<T>);
      property Current: T read GetCurrent;
      function MoveNext: Boolean;
    end;

  function GetEnumerator: TFilteredEnumerator; reintroduce;
  procedure SetFilter(AFilterFunction: TFilterFunction<T>);
  procedure ClearFilter;
  end;

implementation

constructor TFilteredList<T>.TFilteredEnumerator.Create(AList: TFilteredList<T>; AFilterFunction: TFilterFunction<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
  FFilterFunction := AFilterFunction;
end;

function TFilteredList<T>.TFilteredEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TFilteredList<T>.TFilteredEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFilteredList<T>.TFilteredEnumerator.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TFilteredList<T>.TFilteredEnumerator.IsEOL: Boolean;
begin
  Result := Findex >= FList.Count;
end;

function TFilteredList<T>.TFilteredEnumerator.IsLast: Boolean;
begin
  Result := FIndex = FList.Count - 1;
end;

function TFilteredList<T>.TFilteredEnumerator.ShouldIncludeItem: Boolean;
begin
  Result := True;
  if Assigned(FFilterFunction) then
    Result := FFilterFunction(FList[FIndex]);
end;

function TFilteredList<T>.TFilteredEnumerator.MoveNext: Boolean;
begin
  if IsLast then
    Exit(False);

  repeat
    Inc(FIndex);
  until isEOL or ShouldIncludeItem;

  Result := not IsEol;
end;

{ TFilteredList<T> }

procedure TFilteredList<T>.ClearFilter;
begin
  FFilterFunction := nil;
end;

function TFilteredList<T>.GetEnumerator: TFilteredEnumerator;
begin
  Result := TFilteredEnumerator.Create(Self, FFilterFunction);
end;

procedure TFilteredList<T>.SetFilter(AFilterFunction: TFilterFunction<T>);
begin
  FFilterFunction := AFilterFunction;
end;

end.
