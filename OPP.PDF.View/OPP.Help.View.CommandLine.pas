unit OPP.Help.View.CommandLine;

interface

uses
  System.Classes,
  System.JSON, DBXJSONReflect, REST.JSON, System.IOUtils,
  OPP.Help.System.Stream,
  OPP.Help.System.Types,
  FDC.CommandLine,
  OPP.Help.Predicate;

type
  TOPPHelpViewCommandLineCompletion = reference to procedure(const result: TOPPHelpPredicate);

  TOPPHelpViewCommandLine = class(TOptionParser)
  public
    filename: TOption<String>;
    keywordtype: TOption<String>;
    valuetosearch: TOption<String>;
    class function ReadFromCommandLine(completion: TOPPHelpViewCommandLineCompletion): TOPPHelpViewCommandLine;
    constructor Create; override;
  end;

implementation

{ TOPPHelpViewCommandLine }

constructor TOPPHelpViewCommandLine.Create;
begin
  inherited;

  filename := Declare<String>('filename', '');
  keywordtype := Declare<String>('keywordtype', TOPPKeywordType.ktPage.asString);
  valuetosearch := Declare<String>('valuetosearch', '1');
end;

class function TOPPHelpViewCommandLine.ReadFromCommandLine(completion: TOPPHelpViewCommandLineCompletion): TOPPHelpViewCommandLine;
var
  CL: TCommandLine;
  fMap: TOPPHelpPredicate;
  fStream: TMemoryStream;
  jsonObject: TJSONObject;
begin

  result := TOPPHelpViewCommandLine.Create;

  CL := TCommandLine.Create;
  try
    result.Parse(CL.CmdLn);
    fMap := TOPPHelpPredicate.Create;
    try
      fMap.filename := result.filename.Value;
      fMap.keywordtype := TOPPKeywordType.FromString(result.keywordtype.Value);
      fMap.Value := result.valuetosearch.Value;
      if Assigned(completion) then
        completion(fMap);
    finally
      //fMap.Free;
    end;
  finally
    CL.Free;
  end

end;

end.
