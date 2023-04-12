unit SampleFormStubsHelper;

interface

uses
  Datasnap.DBClient, Data.DB;

type
  TSampleFormStubsHelper = class
  public
    class procedure openInternalHelp;
    class procedure openExternalHelp;
    { --- }
    class procedure generateHintMapping;
    { --- }
    class procedure savePredicateToStream;
    class procedure readPredicateFromFile;
    class procedure savePredicateToFile;
  end;

implementation

uses
  System.SysUtils,
  System.Classes, System.Generics.Collections,
  Vcl.Forms,

  OPP.Help.Log,
  SampleOnly.Help.Hint.Setup,
  OPP.Help.Shortcut.Server,
  OPP.Help.System.Types,
  OPP.Help.Predicate,
  OPP.Help.Map,
  OPP.Help.Hint.Server,
  OPP.Help.Map.Parser.JSON;

class procedure TSampleFormStubsHelper.openInternalHelp;
// var
// fPredicate: TOPPHelpPredicate;
// fClassInfo: Pointer;
begin
  // fPredicate := TOPPHelpPredicate.Create();
  // fPredicate.keywordType := ktPage;
  // fPredicate.value := '12';
  // fPredicate.fileName := '.\help\shortcuts\readme.pdf';
  // helpShortcutServer.showHelp(fPredicate, vmInternal, OnShowHelpResult);
end;

class procedure TSampleFormStubsHelper.openExternalHelp;
var
  fPredicate: TOPPHelpPredicate;
  fChild: TOPPHelpPredicate;
begin

  fChild := TOPPHelpPredicate.Create;
  try
    fChild.value := 'начальник цеха, табельщик';
    fChild.keywordType := TOPPKeywordType.ktSearch;

    fPredicate := TOPPHelpPredicate.Create();
    try
      fPredicate.keywordType := ktPage;
      fPredicate.value := '800';
      fPredicate.fileName := 'D:\GulfStream\Compiled\Executable\help\shortcuts\huge_readme.pdf';
      fPredicate.predicates.Add(fChild);
      helpShortcutServer.showHelp(fPredicate, vmExternal,
        procedure(completionResult: TOPPHelpShortcutPresentingResult)
        var
          strmessage: String;
        begin
          strmessage := Format('show help completion result: %d', [Integer(completionResult)]);
          eventLogger.Debug(strmessage);
        end);
    finally
      fPredicate.Free;
    end;
  finally
    fChild.Free;
  end;
end;

class procedure TSampleFormStubsHelper.generateHintMapping;
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm, '.\Документация\hint.idx', '.\Документация\hints.data');
end;

class procedure TSampleFormStubsHelper.savePredicateToStream;
var
  fPredicate, fPredicate2: TOPPHelpPredicate;
  fStream, fStream2: TMemoryStream;
begin

  fStream := TMemoryStream.Create;
  try
    fPredicate := TOPPHelpPredicate.Create;
    try
      fPredicate.value := 'Lorem ipsum';
      fPredicate.keywordType := ktSearch;
      fPredicate.writeToStream(fStream);

      fPredicate2 := TOPPHelpPredicate.Create;
      try
        fPredicate2.readFromStream(fStream, true);
        fStream2 := TMemoryStream.Create;
        try
          fPredicate2.writeToStream(fStream2);
          fStream2.Position := 0;
          eventLogger.Flow('Done', 'SaveToStream');
          //
        finally
          fStream2.Free;
        end;
      finally
        fPredicate2.Free;
      end;

    finally
      fPredicate.Free;
    end;

  finally
    fStream.Free;
  end;
end;

class procedure TSampleFormStubsHelper.readPredicateFromFile;
var
  fList: TList<TOPPHelpMap>;
begin
  fList := TList<TOPPHelpMap>.Create();
  try
    TOPPHelpMapRESTParser.readJSON('.\help\tests\predicates.json',
      procedure(AList: TList<TOPPHelpMap>; error: Exception)
      begin
        fList.addRange(AList);
      end);
  finally
    fList.Free;
  end;
end;

class procedure TSampleFormStubsHelper.savePredicateToFile;
var
  fMap: TOPPHelpMap;
  fPredicate, fChild: TOPPHelpPredicate;
  fList: TList<TOPPHelpMap>;
begin

  fList := TList<TOPPHelpMap>.Create();
  try

    fPredicate := TOPPHelpPredicate.Create;
    fPredicate.value := 'Lorem ipsum';
    fChild := TOPPHelpPredicate.Create;
    fChild.value := 'Lorem ipsum dolor sit';
    fPredicate.predicates.Add(fChild);
    try

      fMap := TOPPHelpMap.Create;
      try
        fMap.Predicate := fPredicate;
        fList.Add(fMap);
        TOPPHelpMapRESTParser.saveJSON(fList, '.\help\tests\predicates.json',
          procedure(AError: Exception)
          begin
          //
          end);
      finally
        fMap.Free;
      end;
    finally
      fPredicate.Free;
    end;
  finally
    fList.Free;
  end;
  //
end;

end.
