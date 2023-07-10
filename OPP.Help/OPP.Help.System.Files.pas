unit OPP.Help.System.Files;

interface

uses WinAPI.Windows;

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD; pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeW';

function AbsToRel(const AbsPath, BasePath: string): string;
function RelToAbs(const RelPath, BasePath: string): string;

type
  TOPPHelpSystemFilesHelper = class
  public
    class function ExeFileDir(): String;
    class function RelativePath(APath: String): String;
    class function AbsolutePath(APath: String): String;
    class function GetOPPSettingsPath(AFileName: String): String;
    class function GetOPPGuidePath(AFileName: String): String;
    class function GetOPPLogsPath(AFileName: String): String;
    class function CreateDirectoryIfNeed(AFileName: String): Boolean;
    class function IsRelativePath(APath: String): Boolean;
    class function FilenameHasPath(AFilename: String):Boolean;
  end;

implementation

uses System.SysUtils,
  System.IOUtils,
  OPP.Help.Log,
  vcl.forms, vcl.dialogs;

class function TOPPHelpSystemFilesHelper.RelativePath(APath: String): String;
begin
  result := AbsToRel(APath, ExeFileDir);
end;

class function TOPPHelpSystemFilesHelper.AbsolutePath(APath: String): String;
begin
  result := RelToAbs(APath, ExeFileDir);
end;

function AbsToRel(const AbsPath, BasePath: string): string;
var
  Path: array [0 .. MAX_PATH - 1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  result := Path;
end;

function RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array [0 .. MAX_PATH - 1] of char;
begin
  PathCanonicalize(@Dst[0], PChar(IncludeTrailingBackslash(BasePath) + RelPath));
  result := Dst;
end;

class function TOPPHelpSystemFilesHelper.CreateDirectoryIfNeed(AFileName: String): Boolean;
var
  fDirectoryPath: String;
begin
  result := false;
  if Length(AFileName) = 0 then
  begin
    eventLogger.Error('Path is empty');
    exit;
  end;

  fDirectoryPath := System.SysUtils.ExtractFilePath(AFileName);

  if System.IOUtils.TDirectory.Exists(fDirectoryPath) then
  begin
    result := true;
    exit;
  end;

  try
    System.IOUtils.TDirectory.CreateDirectory(fDirectoryPath);
    result := true;
  except
    on e: Exception do
    begin
      eventLogger.Error(e);
      ShowMessage(Format('Not able to save file: %s, because %s', [AFileName, e.Message]));
      result := false;
    end;
  end;
end;

class function TOPPHelpSystemFilesHelper.ExeFileDir: String;
begin
  result := ExtractFileDir(Application.ExeName);
end;

class function TOPPHelpSystemFilesHelper.FilenameHasPath(AFilename: String): Boolean;
begin
  result := Length(System.SysUtils.ExtractFilePath(AFileName)) > 0;
end;

class function TOPPHelpSystemFilesHelper.GetOPPGuidePath(AFileName: String): String;
var
  fSettingsPath: String;
begin
  try
    fSettingsPath := TPath.Combine(TPath.GetHomePath, 'Ascon\Gulfstream\Guide');
    TDirectory.CreateDirectory(fSettingsPath);
    result := fSettingsPath + TPath.DirectorySeparatorChar + AFileName;
  except
    result := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
  end;
end;

class function TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFileName: String): String;
var
  fSettingsPath: String;
begin
  try
    fSettingsPath := TPath.Combine(TPath.GetHomePath, 'Ascon\Gulfstream\Settings');
    TDirectory.CreateDirectory(fSettingsPath);
    result := fSettingsPath + TPath.DirectorySeparatorChar + AFileName;
  except
    result := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
  end;
end;

class function TOPPHelpSystemFilesHelper.IsRelativePath(APath: String): Boolean;
var
  valueToCompare: String;
begin
  valueToCompare := RelToAbs(APath, ExeFileDir);
  result := TFile.Exists(valueToCompare)
end;

class function TOPPHelpSystemFilesHelper.GetOPPLogsPath(AFileName: String): String;
var
  fSettingsPath: String;
begin
  try
    fSettingsPath := TPath.Combine(TPath.GetHomePath, 'Ascon\Gulfstream\Logs');
    TDirectory.CreateDirectory(fSettingsPath);
    result := fSettingsPath + TPath.DirectorySeparatorChar + AFileName;
  except
    result := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
  end;
end;

end.
