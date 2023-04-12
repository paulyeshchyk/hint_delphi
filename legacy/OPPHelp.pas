{}unit OPPHelp;

interface
uses sysUtils, { HH, }
  Controls,
  ComObj,
  Windows, { hh_funcs, }
  OPPConstants,
  classes,

  htmlHelpViewer,
//  StoHtmlHelp,


  Opp_Forms_MessageBox
  ;

procedure CHMHelpExecute(HelpContext: integer = 0);
procedure CHM_HelpContext(HelpContext: Integer);
function InitHelp: boolean;
function CanShowTopics(const Topics, sHelpFile: string): integer;

implementation
uses StrUtils, forms;

//Topics (str) - названия топиков через ","
//Result (int) - номер (начинается с 1) доступного топика, 0 - если таковых нет

function CanShowTopics(const Topics, sHelpFile: string): integer;
function IsChmFile(const FileName: string): Boolean;
var
  iPos: Integer;
  sFileExt: string;
begin
  // find extension
  iPos := LastDelimiter('.', FileName);
  if (iPos > 0) then
  begin
    sFileExt := Copy(FileName, iPos, Length(FileName));
    Result := CompareText(sFileExt, '.chm') = 0;
  end
  else
    Result := False;
end;
var
  sFileExt: string;
  sTopic: string;

  Mem: TMemoryStream;
  slTopic: TStringList;
  Temp: array[0..255] of byte;
  i, CurPos, Len: integer;
  FHtmlExt:String;
begin
  FHtmlExt := '.html';
  Result := 0;
  if not IsChmFile(sHelpFile) then exit;
  
  Mem := TMemoryStream.Create;
  slTopic := TStringList.Create;
  try
     slTopic.CommaText := Topics;

     for I := 0 to slTopic.Count - 1 do
     begin
        sTopic := slTopic[i];
        sFileExt := LowerCase(ExtractFileExt(sTopic));

        if (sFileExt <> '.htm') and (sFileExt <> '.html') then
          sTopic := sTopic + FHtmlExt;

        slTopic[i] := '/' + LowerCase(sTopic);
     end;

     Mem.LoadFromFile(sHelpFile);

     ZeroMemory(@Temp, Length(Temp));

     for I := 0 to slTopic.Count - 1 do
     begin
        sTopic := slTopic[I];
        Len := Length(sTopic);
        CurPos := 0;
        Mem.Position := 0;

        if Len = 0 then continue;        

        while Mem.Read(Temp, Len) = Len do
        begin
           if CompareMem(@Temp, @sTopic[1], Len) = True then
           begin
              Result := I + 1;
              exit;
           end;

           inc(CurPos);
           Mem.Position := CurPos;
        end;
     end;
  finally
     FreeAndNil(Mem);
     FreeAndNil(slTopic);
  end;
end;


function InitHelp: boolean;
//var
//  HelpFileName: WideString;
//  appName: string;
begin
//  Result := False;
//  HelpFileName := extractFilePath(ParamStr(0));
//  appName := extractFileName(Paramstr(0));
//  //  if AnsiContainsText(appName,conf_ExeName) then
//  //  HelpFileName := Format('%s%s', [HelpFileName, OPPConstants.Cofigurator_Help]) else
//  if Length(HelpFileName) > 0 then
//    if HelpFileName[Length(HelpFileName)] <> '\' then
//      HelpFileName := HelpFileName + '\';
//
//  HelpFileName := Format('%s%s', [HelpFileName, OPPConstants.Client_Help]);
//  if FileExists(HelpFileName) then
//  begin
//    Application.HelpFile := HelpFileName;
//    Result := True;
//  end
//  else
//    Opp_MessageBox('Файл справки не найден',
//      MB_ICONError);
end;

procedure CHM_HelpContext(HelpContext: Integer);
begin
  try
    InitHelp;
    Application.HelpContext(HelpContext);
  except
    on E: exception do
      Opp_MessageBox(e.Message, MB_ICONError);
  end;
end;

procedure CHMHelpExecute(HelpContext: integer = 0);
  function FindHelpContext(C: TControl): integer;
  var
    i: integer;
  begin
    if C = nil then
      Result := 0
    else if C.HelpContext <> 0 then
      Result := C.HelpContext
    else if C.Parent <> nil then
      Result := FindHelpContext(C.Parent)
    else
      Result := 0;
  end;
begin

  //Из-за ввода новой системы помощи необходимо избавиться от старой методики отображения подсказок
  exit;


  if HelpContext = 0 then
  begin
    if Screen.ActiveForm <> nil then
      HelpContext := FindHelpContext(Screen.ActiveForm.ActiveControl);
    if HelpContext = 0 then
    begin
      HelpContext := FindHelpContext(Screen.ActiveForm);
      if HelpContext = 0 then
      begin
        HelpContext := FindHelpContext(Application.MainForm.ActiveControl);
        if HelpContext = 0 then
          HelpContext := FindHelpContext(Application.MainForm);
      end;
    end;
  end;
  try
    OPPHelp.CHM_HelpContext(HelpContext);
  except
    try
      OPPHelp.CHM_HelpContext(0);
    except
    end;
  end;
end;
end.

