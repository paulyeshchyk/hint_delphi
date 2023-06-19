unit FDC.CommandLine;

///  <summary>
///  Written by Lars Fosdal, August 2019, Delphi 10.3.1
///  Your license to use and modify follows the Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) license rules.
///  https://creativecommons.org/licenses/by-sa/4.0/
///  </summary>

interface
uses
  System.Classes, System.StrUtils, System.SysUtils, System.SyncObjs,
  System.Generics.Defaults, System.Generics.Collections, System.RTTI, System.TypInfo;

type
  /// <summary> DebugOut handler </summary>
  TCmdDebugOutHandler = reference to procedure(const aMsg: string);

  /// <summary> Setting the current DebugOut handler </summary>
  procedure SetCmdDebugOutHandler(const aHandler: TCmdDebugOutHandler);

  /// <summary> Output a debug string in this unit </summary>
  procedure CmdDebugOut(const aMsg: string);


type
  /// <summary> A value assigned to an option </summary>
  TParam = class abstract
  protected
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const aValue: string); virtual; abstract;
    function Debug: string;
  public
    constructor Create; virtual; abstract;
    property AsString: string read GetAsString write SetAsString;
  end;

  const
   /// <summary> The characters recognized as an option switch </summary>
   CmdSwitch: TSysCharSet = ['-', '+', '/'];
   /// <summary> The characters allowed in an option name </summary>
   CmdName: TSysCharSet   = ['a'..'z', 'A'..'Z', '0'..'9', '_', '?', '&', '#', '%'];
   /// <summary> The characters recognized as whitespace </summary>
   CmdSpace: TSysCharSet  = [' ', ^I];
   /// <summary> The characters allowed as a flag after an option </summary>
   CmdFlag: TSysCharSet   = ['+', '-', '=', ':'];
   /// <summary> The characters recognized as list separators </summary>
   CmdList: TSysCharSet   = [';', ','];

type
  ///  <summary> An option may contain multiple parameters
  ///  <code>/option1=" a parameter " /option2=(param1, param2, "param 3") /option3+ --option4-</code>
  ///  </summary>
  TOption = class abstract
  private
    FName: string;
    FSwitch: string;
    FFlag: string;
    FGiven: Boolean;
    FOrder: Integer;
  protected
    function Add: TParam; virtual; abstract;
    function Duplicate: string; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    /// <summary> Clears values of options, but does not remove them - unlike Clear </summary>
    procedure ClearValues; virtual; abstract;
    /// <summary> Option debug info </summary>
    function Debug: string; virtual; abstract;
    /// <summary> First parameter value </summary>
    function AsString: string; virtual; abstract;
    /// <summary> All parameter values as string </summary>
    function AsStringList: TArray<string>; virtual; abstract;
    /// <summary> Option name as in /option </summary>
    property Name: string read FName write FName;
    /// <summary> Leading switch(es) </summary>
    property Switch: string read FSwitch write FSwitch;
    /// <summary> Trailing flag(s) </summary>
    property Flag: string read FFlag write FFlag;
    /// <summary> True if found in parsing </summary>
    property Given: Boolean read FGiven write FGiven;
    /// <summary> Order in parsed string </summary>
    property Order: Integer read FOrder write FOrder;

  end;

  /// <summary> Generic parameter value of type T</summary>
  TParam<T> = class(TParam)
  private
    FValue: T;
    FParent: TOption;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const aValue: string); override;
    property Parent: TOption read FParent write FParent;
  public
    constructor Create; override;
    property Value: T read FValue write FValue;
  end;

  /// <summary><para>Option containing parameters of type T. The default type of unknown options is String.</para>
  /// <para>This can be extended by adding predefined typed options before parsing.</para>
  /// <para>Supports string, int, float, and enumerated types.</para></summary>
  TOption<T> = class(TOption)
  type
    TParamList = class(TObjectList<TParam<T>>);
  private
    FParams: TParamList;
    FDefaultValue: T;
    function GetValue: T;
    function GetValueList: TArray<T>;
    function Analyzed: string;
  protected
    function Add: TParam; override;
    function Duplicate: string; override;
    property Params: TParamList read FParams;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearValues; override;
    function Debug: string; override;
    function AsString: string; override;
    function AsStringList: TArray<string>; override;
    function MultiParams: Boolean;
    property DefaultValue: T read FDefaultValue write FDefaultValue;
    property Value: T read GetValue;
    property ValueList: TArray<T> read GetValueList;
  end;

  /// <summary> The actual parser that collects options and their parameters </summary>
  TOptionParser = class(TObjectDictionary<string, TOption>)
  type
    TOptionComparer = class(TComparer<TOption>)
      function Compare(const Left, Right: TOption): Integer; override;
    end;
  public
    constructor Create; reintroduce; virtual;
    procedure ClearValues;
    function Declare<T>(const aName: String; const aDefaultValue: T): TOption<T>; overload;
    procedure Declare(const aName: String; aOption: TOption); overload;
    function Duplicate: string;
    function DuplicateExcluding(const aNames: TArray<String>): String;
    procedure Parse(aOptions: string);
    function Given(const aName: string): boolean;
    function Option<T>(const aName: string): TOption<T>;
    function AsString(const aName: string; const aDefault: string = ''): string;
    function AsStringList(const aName: string): TArray<string>;
    procedure Debug(const Strings: TStrings);
  end;

  /// <summary> Concatenates all ParamStr entries and parses the command line </summary>
  TCommandLine = class(TOptionParser)
  private
    FCmdLn: string;
  public
    constructor Create; override;
    property CmdLn: string read FCmdLn;
  end;

  /// <summary> Singleton of the current commandline ready parsed </summary>
  function CommandLine: TCommandLine;
  /// <summary> Lookup a specific option in the current commandline </summary>
  function CommandLineStr(const aName:String; const aDefault:String = ''): string;

implementation

var
  CallDebugOut: TCmdDebugOutHandler;

procedure CmdDebugOut(const aMsg: string);
begin
  CallDebugOut(aMsg);
end;

procedure SetCmdDebugOutHandler(const aHandler: TCmdDebugOutHandler);
begin
  CallDebugOut := aHandler;
end;

procedure NoOutput(const aMsg: string);
begin
  // sends aMsg nowhere :P
end;

{ TParam }

function TParam.Debug: string;
begin
  Result := AsString;
  if Pos(' ', Result) > 0
  then begin
    if Pos('"', Result)> 0
     then Result := '''' + Result + ''''
      else Result := '"' + Result + '"';
  end;
end;

{ TParam<T> }

constructor TParam<T>.Create;
begin
  inherited;
  FValue := Default(T);
end;

function TParam<T>.GetAsString: string;
var
  TV: TValue;
begin
  TV := TValue.From<T>(FValue);
  Result := TV.AsString;
end;

procedure TParam<T>.SetAsString(const aValue: string);
var
  TV: TValue;
begin
  TV := TValue.From<T>(Default(T));
  try
    case TV.Kind of
      tkEnumeration: TV := TValue.FromOrdinal(TypeInfo(T), GetEnumValue(TypeInfo(T), aValue));
      tkInteger: TV := TValue.From<Integer>(StrToInt(aValue));
      tkInt64:   TV := TValue.From<Int64>(StrToInt(aValue));
      tkFloat:   TV := TValue.From<Extended>(StrToFloat(aValue));
            else TV := TValue.From<String>(aValue);
    end;
    FValue := TV.AsType<T>;
  except
    on E:Exception
    do begin
      CmdDebugOut(Parent.Debug + ': "' + aValue + '" -> ' + E.Message);
      FValue := Default(T);
    end;
  end;
end;

{ TOptionParser.TOptionComparer }

function TOptionParser.TOptionComparer.Compare(const Left, Right: TOption): Integer;
begin
  if (Left.Order < Right.Order)
   then Result := -1
  else if (Left.Order > Right.Order)
   then Result := 1
  else Result := 0;
end;

{ TOption<T> }

procedure TOption<T>.ClearValues;
begin
  FGiven := False;
  FParams.Clear;
end;

constructor TOption<T>.Create;
begin
  inherited;
  FName := '';
  FSwitch := '';
  FFlag := '';
  FGiven := False;
  FOrder := 0;
  FDefaultValue := Default(T);
  FParams := TParamList.Create;
end;

destructor TOption<T>.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TOption<T>.Add: TParam;
var p: TParam<T>;
begin
  p := TParam<T>.Create;
  p.Parent := Self;
  Result := p;
  FParams.Add(p);
end;

function TOption<T>.Duplicate: string;
var
  ParamValues, Semi: string;
  p: TParam<T>;
begin
  ParamValues := '';
  if Params.Count = 1
  then ParamValues := Params[0].Debug
  else begin
    semi := '';
    for p in params
    do begin
      ParamValues := ParamValues + Semi + p.Debug;
      semi := ';'
    end;
    if ParamValues <> ''
     then ParamValues := '(' + ParamValues + ')';
  end;
  Result := Switch + Name + Flag + ParamValues;
end;

function TOption<T>.Analyzed: string;
var
  ParamValues, Semi: string;
  p: TParam<T>;
begin
  ParamValues := '';
  if Params.Count = 1
  then ParamValues := Params[0].Debug
  else begin
    semi := '';
    for p in params
    do begin
      ParamValues := ParamValues + Semi + p.Debug;
      semi := ';'
    end;
    if ParamValues <> ''
     then ParamValues := '(' + ParamValues + ')';
  end;
  Result := Format('Switch[%s] Option[%s] Flag[%s] Values[%s]',
    [Switch, Name, Flag, ParamValues]);
end;

function TOption<T>.Debug: string;
begin
  Result := Order.ToString + ' ' + Analyzed;
end;

function TOption<T>.AsString: string;
begin
  if Params.Count > 0
   then Result := Params[0].AsString
    else Result := '';
end;

function TOption<T>.AsStringList: TArray<string>;
var p: TParam<T>;
begin
  Result := [];
  if Params.Count > 0
  then begin
    for p in Params
    do Result := Result + [p.AsString];
  end;
end;

function TOption<T>.GetValue: T;
begin
  if Params.Count > 0
  then Result := Params[0].Value
  else Result := DefaultValue;
end;

function TOption<T>.GetValueList: TArray<T>;
var ix: Integer;
begin
  SetLength(Result, Params.Count);
  for ix := 0 to Params.Count - 1
   do Result[ix] := Params[ix].Value;
end;

function TOption<T>.MultiParams: Boolean;
begin
  Result := Params.Count > 1;
end;

{ TOptionParser }

function TOptionParser.AsString(const aName: string; const aDefault: string = ''): string;
var
  Option: TOption;
begin
  if TryGetValue(LowerCase(aName), Option)
   then Result := Option.AsString
    else Result := '';
  if Result = ''
   then Result := aDefault;
end;

function TOptionParser.AsStringList(const aName: string): TArray<string>;
var
  Option: TOption;
begin
  if TryGetValue(LowerCase(aName), Option)
   then Result := Option.AsStringList
    else Result := [];
end;

constructor TOptionParser.Create;
begin
  Inherited Create([doOwnsValues], 19);
end;

procedure TOptionParser.ClearValues;
var Option: TOption;
begin
  for Option in Values
   do Option.ClearValues;
end;

procedure TOptionParser.Debug(const Strings: TStrings);
var Option: TOption;
begin
  for Option in Values
   do Strings.Add(Option.Debug);
end;

procedure TOptionParser.Declare(const aName: String; aOption: TOption);
begin
  aOption.Name := aName;
  Add(LowerCase(aName), aOption);
end;

function TOptionParser.Declare<T>(const aName: String; const aDefaultValue: T): TOption<T>;
begin
  Result := TOption<T>.Create;
  Result.DefaultValue := aDefaultValue;
  Declare(aName, Result);
end;

function TOptionParser.Duplicate: string;
begin
  Result := DuplicateExcluding([]);
end;

function TOptionParser.DuplicateExcluding(const aNames: TArray<String>): String;
var
  List: TList<TOption>;
  opt: TOption;
  comp: TOptionComparer;
  name: String;
begin
  List := TList<TOption>.Create;
  try
    for opt in self.Values
    do begin
      for name in aNames
      do if CompareText(name, opt.Name) = 0
       then Continue;
      List.Add(opt);
    end;
    comp := TOptionComparer.Create;
    try
      List.Sort(comp);
    finally
      comp.Free;
    end;
    Result := '';
    for opt in List
    do Result := Result + ' ' + opt.Duplicate;
  finally
    List.Free;
  end;
end;

function TOptionParser.Given(const aName: string): boolean;
var
  Option: TOption;
begin
  if TryGetValue(LowerCase(aName), Option)
   then Result := Option.Given
    else Result := False;
end;

function TOptionParser.Option<T>(const aName: string): TOption<T>;
var
  Option: TOption;
begin
  if TryGetValue(LowerCase(aName), Option)
   then Result := Option as TOption<T>
    else Result := nil;
end;

///  Without arguments: option /option -option --option
///
///  With flag: option- /option+ -option+ --option-
///
///  With single value: option:value /option : value -option= 0  --option: 2019.04.01
///
///  With quoted values - any character can be allowed in a quoted string until a matching end quote is found
///  option:"This is 'a' string"   /option: 'This is "a" string'
///
///  With a list of values: option:("some filename" ; 'another file name')

procedure TOptionParser.Parse(aOptions: string);

  function Get(chSet: TSysCharSet): String;
  var p: Integer;
      ch: Char;
  begin
    Result := '';
    p := 1;
    while (p <= Length(aOptions)) and CharInSet(aOptions[p], chSet)
    do begin
      ch := aOptions[p];
      if (ch <> ' ')
       then Result := Result + ch;
      Inc(p);
    end;
    Delete(aOptions, 1, p - 1);
  end;

  function Grab(aQuote: Char): String;
  var p: Integer;
  begin
    if aOptions[1] = aQuote
    then begin
      Delete(aOptions, 1,1);
      p := Pos(aQuote, aOptions);
      if p <= 0
      then p := Length(aOptions) + 1;
      Result := Copy(aOptions, 1, p - 1);
      Delete(aOptions, 1, p);
    end;
  end;

  function GrabUntil(chSet: TSysCharSet): String;
  var p, l: Integer;
      ch: Char;
  begin
    Result := '';
    p := 1;
    ch := #0;
    l := Length(aOptions);
    if  l > 0
     then ch := aOptions[1];

    while (p <= l) and not CharInSet(ch, chSet)
    do begin
      Result := Result + ch;
      Inc(p);
      if  p <= l
       then ch := aOptions[p];
    end;
    Delete(aOptions, 1, Length(Result));
  end;

  function Peek: string;
  begin
    if aOptions <> ''
     then Result := aOptions[1]
      else Result := '';
  end;

var
  switch, name, flag, value, LastOptions: string;
  Option: TOption;
  param: TParam;
begin
  ClearValues;

  aOptions := Trim(aOptions);
  while aOptions <> ''
  do begin
    LastOptions := aOptions;
    {$ifdef debug}
//    CmdDebugOut('Parse:' + aOptions);
    {$endif}
    switch := Get(CmdSwitch);
    if Peek = ' '
     then Get(CmdSpace);

    name := Get(CmdName);
    flag := '';
    if Peek <> ' '
     then flag := Get(CmdFlag)
    else Get(CmdSpace);
    if (Peek = ':') or (Peek = '=')
     then flag := Get(CmdFlag);
    Get(CmdSpace);

    if (name <> '') and not TryGetValue(LowerCase(name), Option)
    then begin
      Option := Declare<String>(name, '');
      Option.Order := Self.Count;
    end;
    Option.Switch := switch;
    Option.Flag := flag;
    Option.Given := True;

    if (Pos(':', flag) + Pos('=', Flag)) > 0  // a parameter has been declared
    then begin
      if Peek = '('
      then begin // array of arguments
        Get(['('] + CmdSpace);
        repeat
          if Peek = '"'
           then value := Grab('"')
          else if Peek = ''''
           then value := Grab('''')
          else value := Trim(GrabUntil([')'] + CmdList));

          if Value <> ''
          then begin
            param := Option.Add;
            param.AsString := value;
          end;
          Get(CmdList + CmdSpace);

        until (Peek = ')') or (Peek = '');
        Get([')'] + CmdSpace);

      end  // single argument
      else begin
        if Peek = '"'
         then value := Grab('"')
        else if Peek = ''''
         then value := Grab('''')
        else value := Trim(GrabUntil(CmdSpace));

        if Value <> ''
        then begin
          param := Option.Add;
          param.AsString := value;
        end;
      end;

    end;

    Get(CmdSpace);
    if LastOptions = aOptions // nothing was consumed,
     then Delete(aOptions, 1, 1); // so deal with unexpected characters that cause a loop

    aOptions := Trim(aOptions);
  end;
end;

{ TCommandLine }

constructor TCommandLine.Create;
var ix: Integer;
var SL : TStringList;
begin
  inherited;
  FCmdLn := '';
  for ix := 1 to ParamCount
   do FCmdLn := FCmdLn + ParamStr(ix) + ' ';
  Parse(CmdLn);
{$ifdef debug}
  CmdDebugOut('Org: ' + CmdLn);
  CmdDebugOut('Dup: ' + Duplicate);
  SL := TStringList.Create;
  try
    Debug(SL);
    CmdDebugOut(SL.Text);
  finally
    SL.Free;
  end;
{$endif}
end;


var
  CommandLineSection : TCriticalSection;
  CurrentCommandLine : TCommandLine;

function CommandLine:TCommandLine;
begin
  CommandLineSection.Acquire;
  try
    if not Assigned(CurrentCommandLine)
     then CurrentCommandLine := TCommandLine.Create;
    Result := CurrentCommandLine;
  finally
    CommandLineSection.Release;
  end;
end;

function CommandLineStr(const aName:String; const aDefault:String):String;
begin
  Result := CommandLine.AsString(aName, aDefault);
end;


initialization
  SetCmdDebugOutHandler(NoOutput);

  CommandLineSection := TCriticalSection.Create;
  CurrentCommandLine := nil;

finalization
  CommandLineSection.Acquire;
  try
    CurrentCommandLine.Free;
  finally
    CommandLineSection.Release;
    CommandLineSection.Free;
  end;

end.
