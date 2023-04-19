﻿unit OPP.Help.Component.Enumerator;

interface

uses
  system.classes, system.sysUtils, system.TypInfo, system.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls,
  OPP.Help.Meta;

type

  TOPPHelpComponentComparator = reference to function(AComponent: TComponent): Boolean;
  TOPPHelpComponentEnumeratorCompletion = reference to procedure(AComponent: TComponent);

  TOPPHelpComponentEnumerator = class helper for TComponent
  public
    function GetChildrenRecursive(AComparator: TOPPHelpComponentComparator = nil; ACompletion: TOPPHelpComponentEnumeratorCompletion = nil): TList<TComponent>;
    function FindSubControl(Meta: TOPPHelpMeta): TComponent;
    function isSupportingMeta(Meta: TOPPHelpMeta): Boolean;
    function canApplyHintsRecursively(): Boolean;
  end;

implementation

uses OPP.Help.Log;

function TOPPHelpComponentEnumerator.GetChildrenRecursive(AComparator: TOPPHelpComponentComparator; ACompletion: TOPPHelpComponentEnumeratorCompletion): TList<TComponent>;
var
  fChild: TComponent;
  i: Integer;
  fComparatorResult: Boolean;
  fChildren: TList<TComponent>;
begin
  result := TList<TComponent>.Create();

  for i := 0 to ComponentCount - 1 do
  begin
    fChild := self.Components[i];

    eventLogger.Debug(Format('enumerating [%s]', [fChild.ClassName]),'Enumerator');

    if assigned(AComparator) then
    begin
      fComparatorResult := AComparator(fChild);
      if fComparatorResult = true then
      begin
        eventLogger.Debug(Format('adding component [%s] with comparator', [fChild.ClassName]),'Enumerator');
        result.Add(fChild);
        if assigned(ACompletion) then
          ACompletion(fChild);
      end;

      // recursion: add all children, even parent is not valid
      fChildren := fChild.GetChildrenRecursive(AComparator, ACompletion);
      try
        result.AddRange(fChildren);
      finally
        fChildren.Free;
      end;

    end else begin
      eventLogger.Debug(Format('adding component [%s] w/o comparator', [fChild.ClassName]),'Enumerator');
      result.Add(fChild);
      if assigned(ACompletion) then
        ACompletion(fChild);

      // recursion
      fChildren := fChild.GetChildrenRecursive(AComparator, ACompletion);
      try
        result.AddRange(fChildren);
      finally
        fChildren.Free;
      end;

    end;
  end;
end;

function TOPPHelpComponentEnumerator.isSupportingMeta(Meta: TOPPHelpMeta): Boolean;
var
  valueToCompare: String;
begin
  result := false;
  if (IsPublishedProp(self, Meta.propertyName)) then
  begin
    valueToCompare := String(GetPropValue(self, Meta.propertyName));
    result := CompareStr(valueToCompare, Meta.identifier) = 0;
  end;
end;

function TOPPHelpComponentEnumerator.canApplyHintsRecursively(): Boolean;
const
  fListOfClasses: array of String = ['TOppAttrControl', 'TOppObjControl'];
var
  i: Integer;
begin
  result := false;
  for i := 0 to Length(fListOfClasses) - 1 do
  begin
    result := self.ClassName = fListOfClasses[i];
    if result then
      break;
  end;
end;

function TOPPHelpComponentEnumerator.FindSubControl(Meta: TOPPHelpMeta): TComponent;
var
  child: TComponent;
  fChildren: TList<TComponent>;
begin

  result := nil;

  fChildren := self.GetChildrenRecursive(
    function(AComponent: TComponent): Boolean
    begin
      result := true;
    end,
    procedure(AComponent: TComponent)
    begin
      //
    end);

  for child in fChildren do
  begin
    // eventLogger.Debug(Format('Enumerator: Form [%s] Iterating over component [%s]', [self.ClassName, child.ClassName]));

    if child.isSupportingMeta(Meta) then
    begin
      result := child;
      break;
    end;

  end;

  //fChildren.Free;
end;

end.
