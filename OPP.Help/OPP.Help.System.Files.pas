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
    class function RelativePath(APath: String): String;
    class function AbsolutePath(APath: String): String;
    class function GetOPPSettingsPath(AFileName: String): String;
  end;

implementation

uses System.SysUtils,
  System.IOUtils,
  vcl.forms;

class function TOPPHelpSystemFilesHelper.RelativePath(APath: String): String;
begin
  result := AbsToRel(APath, ExtractFileDir(Application.ExeName));
end;

class function TOPPHelpSystemFilesHelper.AbsolutePath(APath: String): String;
begin
  result := RelToAbs(APath, ExtractFileDir(Application.ExeName));
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

class function TOPPHelpSystemFilesHelper.GetOPPSettingsPath(AFilename: String): String;
var
  fSettingsPath: String;
begin
  try
    fSettingsPath := TPath.Combine(TPath.GetHomePath, 'OPP\Settings');
    TDirectory.CreateDirectory(fSettingsPath);
    result := fSettingsPath + TPath.DirectorySeparatorChar + AFilename;
  except
    result := TOPPHelpSystemFilesHelper.AbsolutePath(AFileName);
  end;
end;

end.
