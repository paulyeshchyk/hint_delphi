unit OPP.Help.System.Application;

interface

uses Vcl.Forms, System.SysUtils, System.Types, WinAPI.Windows, ShellAPI;

type
  TOPPHelpAllicationHelper = class helper for TApplication
    function BuildNumber: String;
    procedure openMarketingWebPage();
  end;

implementation

resourcestring
  SLinkAsconMarketingPage = 'https://ascon.ru/products/golfstrim/';

  { TOPPHelpAllicationHelper }

function TOPPHelpAllicationHelper.BuildNumber: String;
  procedure GetBuildInfo(var V1, V2, V3, V4: word);
  var
    VerInfoSize, VerValueSize, Dummy: DWORD;
    VerInfo: Pointer;
    VerValue: PVSFixedFileInfo;
  begin
    VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
    if VerInfoSize > 0 then
    begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
        begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          with VerValue^ do
          begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
    end;
  end;

var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := Format('%s (%d.%d.%d.%d)', [Self.Title, V1, V2, V3, V4]);
end;

procedure TOPPHelpAllicationHelper.openMarketingWebPage;
var
  URL: string;
begin
  URL := SLinkAsconMarketingPage;
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

end.
