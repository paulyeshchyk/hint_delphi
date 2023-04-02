unit sampleFormHelper;

interface

uses
  Datasnap.DBClient, Data.DB;

type
  TSampleFormHelper = class
  public
    class procedure openInternalHelp;
    class procedure openExternalHelp;
    { --- }
    class procedure generateHintMapping;
    { --- }
    class procedure copyPredicate;
    class procedure savePredicateToStream;
    class procedure readPredicateFromFile;
    class procedure savePredicateToFile;
    { --- }
    class procedure makeRecordsDataset(ADataset: TClientDataSet);
    class procedure makePredicatesDataset(ADataset: TClientDataSet);

    class procedure loadPredicatesDataset(ADataset: TClientDataSet; ARecordID: String);

  end;

implementation

uses
  System.SysUtils,
  System.Classes, System.Generics.Collections,
  Vcl.Forms,

  OPP.Help.Log,
  SampleOnly.Help.Hint.Setup,
  OPP.Help.Shortcut.Server,
  OPP.Help.Nonatomic,
  OPP.Help.Predicate,
  OPP.Help.Map, OPP.Help.Map.Filereader,

  OPP.Help.Hint.Server
  ;

class procedure TSampleFormHelper.openInternalHelp;
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

class procedure TSampleFormHelper.openExternalHelp;
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
      fPredicate.fileName := 'D:\Compiled\Executable\help\shortcuts\huge_readme.pdf';
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

class procedure TSampleFormHelper.generateHintMapping;
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm, '.\help\mapping\hints_matrix__.json', '.\help\hints\gulfstream_manual_rtf.rtf');
end;

class procedure TSampleFormHelper.copyPredicate;
var
  fPredicate, fCopy: TOPPHelpPredicate;
begin

  fPredicate := TOPPHelpPredicate.Create;
  try
    fPredicate.value := 'Lorem ipsum';
    fPredicate.keywordType := ktSearch;
    fCopy := fPredicate.copy();
  finally
    fCopy.Free;
    fPredicate.Free;
  end;
end;

class procedure TSampleFormHelper.savePredicateToStream;
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

class procedure TSampleFormHelper.readPredicateFromFile;
var
  fList: TList<TOPPHelpMap>;
begin
  fList := TList<TOPPHelpMap>.Create();
  try
    TOPPHelpMap.readJSON('.\help\tests\predicates.json',
      procedure(AList: TList<TOPPHelpMap>; error: Exception)
      begin
        fList.addRange(AList);
      end);
  finally
    fList.Free;
  end;
end;

class procedure TSampleFormHelper.savePredicateToFile;
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
    fPredicate.predicates.add(fChild);
    try

      fMap := TOPPHelpMap.Create;
      try
        fMap.Predicate := fPredicate;
        fList.add(fMap);
        TOPPHelpMap.saveJSON(fList, '.\help\tests\predicates.json');
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

class procedure TSampleFormHelper.makePredicatesDataset(ADataset: TClientDataSet);
begin
  if not assigned(ADataset) then begin
    eventLogger.Error('Dataset is not assigned');
  end;

  helpHintServer.makePredicatesDataset(ADataset);

end;

class procedure TSampleFormHelper.makeRecordsDataset(ADataset: TClientDataSet);
begin
  if not assigned(ADataset) then begin
    eventLogger.Error('Dataset is not assigned');
  end;

  helpHintServer.makeRecordsDataset(ADataset);
  helpHintServer.loadRecordsDataset(ADataset);
end;

class procedure TSampleFormHelper.loadPredicatesDataset(ADataset: TClientDataSet; ARecordID: String);
begin
  if not assigned(ADataset) then begin
    eventLogger.Error('Dataset is not assigned');
  end;

  helpHintServer.loadPredicatesDataset(ADataset, ARecordID);
end;

end.
