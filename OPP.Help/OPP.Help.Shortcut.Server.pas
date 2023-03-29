unit OPP.Help.Shortcut.Server;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.SyncObjs, System.Classes,

  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,
  OPP.Help.Predicate,
  OPP.Help.Shortcut.Dataset,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Mapping,
  OPP.Help.System.Messaging,
  OPP.Help.System.Messaging.Pipe;

type

  IOPPHelpShortcutViewer = interface
    ['{097D4F69-916A-4CB4-AB5F-E88D9BA1BB76}']
    procedure runPredicate(APredicate: TOPPHelpPredicate);
    procedure PresentModal;
  end;

  TOPPHelpViewMode = (vmInternal, vmExternal);

  TOPPHelpShortcutPresentingResult = (prSuccess, prFail);
  TOPPHelpShortcutPresentingCompletion = reference to procedure(APresentingResult: TOPPHelpShortcutPresentingResult);
  TOPPHelpShortcutOnGetIdentifier = reference to function(AControl: TControl): String;

  IOPPHelpShortcutServer = interface
    function exportControl(AControl: TControl): Boolean;
    procedure showHelp(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(AViewerClassInfo: Pointer; ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion; onGetIdentifier: TOPPHelpShortcutOnGetIdentifier); overload;
    function loadPDF(AFileName: String): TMemoryStream;
    procedure killExternalViewer();
  end;

  TOPPHelpShortcutServer = class(TComponent, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;

    procedure openInternalViewer(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    procedure openExternalViewer(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    function sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
    /// <remarks> copyright https://stackoverflow.com/a/12949757 </remarks>
    function ForceForegroundWindow(hwnd: THandle): Boolean;
  public
    function loadPDF(AFileName: String): TMemoryStream;
    function exportControl(AControl: TControl): Boolean;
    procedure killExternalViewer();

    procedure showHelp(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(AViewerClassInfo: Pointer; ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion; onGetIdentifier: TOPPHelpShortcutOnGetIdentifier); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

procedure Register;

implementation

uses
  OPP.Help.System.AppExecutor,
  OPP.Help.System.Str,
  OPP.Help.Log,
  System.TypInfo;

const
  shortcutJSONFileName: String = 'help\mapping\shortcut_matrix.json';
  OPPViewerProcessName: String = 'OPPHelpPreview.exe';

var
  fLock: TCriticalSection;
  fHelpServer: IOPPHelpShortcutServer;

function helpShortcutServer: IOPPHelpShortcutServer;
begin
  fLock.Acquire;
  try
    if not Assigned(fHelpServer) then
    begin
      fHelpServer := TOPPHelpShortcutServer.Create(nil);
    end;
    result := fHelpServer;
  finally
    fLock.Release;
  end;
end;

// ---

constructor TOPPHelpShortcutServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fPDFMemoryStream := TDictionary<String, TMemoryStream>.Create;

  fShortcutDataset := TOPPHelpShortcutDataset.Create;
  fShortcutDataset.load(shortcutJSONFileName);
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fShortcutDataset.Free;
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

function TOPPHelpShortcutServer.loadPDF(AFileName: String): TMemoryStream;
var
  fFileNameHash: String;
  fStream: TMemoryStream;
begin
  fFileNameHash := AFileName.hashString;
  fPDFMemoryStream.TryGetValue(fFileNameHash, fStream);
  if not Assigned(fStream) then
  begin
    fStream := TMemoryStream.Create;
    try
      fStream.loadFromFile(AFileName);
      fStream.Position := 0;
      fPDFMemoryStream.Add(fFileNameHash, fStream);
    finally
    end;

  end;
  result := fStream;
end;

procedure TOPPHelpShortcutServer.showHelp(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion);
begin
  case viewMode of
    vmInternal:
      openInternalViewer(AViewerClassInfo, APredicate, completion);
    vmExternal:
      openExternalViewer(AViewerClassInfo, APredicate, completion);
  end;
end;

procedure TOPPHelpShortcutServer.showHelp(AViewerClassInfo: Pointer; ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion; onGetIdentifier: TOPPHelpShortcutOnGetIdentifier);
var
  fMapping: TOPPHelpShortcutMap;
  fShortcutIdentifier: String;
begin

  if not Assigned(onGetIdentifier) then
  begin
    eventLogger.Log('shortcutserver - onGetIdentifier is not defined', lmError);
    exit;
  end;
  fShortcutIdentifier := onGetIdentifier(ARequest.ActiveControl);

  fMapping := fShortcutDataset.GetMapping(fShortcutIdentifier);
  if not Assigned(fMapping) then
  begin
    if Assigned(completion) then
      completion(prFail);
    exit;
  end;

  showHelp(AViewerClassInfo, fMapping.Predicate, viewMode, completion);
end;

function TOPPHelpShortcutServer.exportControl(AControl: TControl): Boolean;
begin
  result := true;
end;

procedure TOPPHelpShortcutServer.openInternalViewer(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
var
  result: Boolean;
  fViewer: TForm;
  clazzType:TClass;
begin
  result := Assigned(APredicate);
  if not result then
  begin
    if Assigned(completion) then
      completion(prFail);
    exit;
  end else begin

    clazzType := GetTypeData(AViewerClassInfo).ClassType;
    if not clazzType.InheritsFrom(TForm) then begin
      eventLogger.Log('viewer is not supporting TFormClass', lmError);
      exit;
    end;

    if not Supports(clazzType, IOPPHelpShortcutViewer) then begin
      eventLogger.Log('viewer is not supporting IOPPHelpShortcutViewer', lmError);
      exit;
    end;

    fViewer := TFormClass(clazzType).Create(nil);
    if not assigned(fViewer) then begin
      eventLogger.Log('viewer was not created', lmError);
      exit;
    end;


    try
      (fViewer as IOPPHelpShortcutViewer).RunPredicate(APredicate);
      (fViewer as IOPPHelpShortcutViewer).PresentModal;
    finally
      FreeAndNil(fViewer);
    end;

    if Assigned(completion) then
      completion(prSuccess);

  end;
end;

procedure TOPPHelpShortcutServer.openExternalViewer(AViewerClassInfo: Pointer; APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
begin
  TOPPHelpSystemAppExecutor.Execute(OPPViewerProcessName, AViewerClassInfo,
    procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType)
    var
      hwnd: THandle;
    begin
      if not(Assigned(AList) and (AList.Count <> 0)) then
      begin
        if Assigned(completion) then
          completion(prFail);
      end else begin

        for hwnd in AList do
        begin
          sendOpenPage(hwnd, APredicate);
          ForceForegroundWindow(hwnd);
        end;

        if Assigned(completion) then
          completion(prSuccess);
      end;
    end);
end;

function TOPPHelpShortcutServer.sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
begin
  result := -1;
  if AProcessHandle = 0 then
  begin
    // ShowMessage('Невозможно запустить окно помощи.');
    exit;
  end;

  fSelfHandle := Application.Handle;

  fMessagePipe := TOPPMessagePipe.Create;
  try
    result := fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
      procedure(AStream: TStream)
      begin
        Predicate.writeToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;

function TOPPHelpShortcutServer.ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then
    ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then
    result := true
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion > 0)))) then
    begin
      result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadProcessID(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, true) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        result := (GetForegroundWindow = hwnd);
      end;
      if not result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end else begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    result := (GetForegroundWindow = hwnd);
  end;
end;

procedure TOPPHelpShortcutServer.killExternalViewer();
begin
  TOPPSystemMessageHelper.KillProcess(OPPViewerProcessName);
end;

procedure Register;
begin
  RegisterComponents('OPPHelp', [TOPPHelpShortcutServer])
end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
