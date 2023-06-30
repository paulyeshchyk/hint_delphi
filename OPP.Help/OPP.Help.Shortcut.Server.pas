unit OPP.Help.Shortcut.Server;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.SyncObjs, System.Classes,

  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,

  OPP.Help.System.References,
  OPP.Help.Interfaces,

  OPP.Help.Predicate,
  OPP.Help.Shortcut.Dataset,
  OPP.Help.Shortcut.Request,
  OPP.Help.Map,
  OPP.Help.System.Messaging,
  OPP.Help.System.Messaging.Pipe;

type

  TOPPHelpViewMode = (vmInternal, vmExternal);

  TOPPHelpShortcutPresentingCompletion = reference to procedure(Error: Exception);
  TOPPHelpShortcutOnGetIdentifier = reference to function(AControl: TControl): String;
  TOPPHelpShortcutServerLoadStreamStatus = (ssCreated, ssReused, ssError);
  TOPPHelpShortcutServerLoadStreamCompletion = reference to procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus);

  IOPPHelpShortcutServer = interface
    procedure ShowHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure ShowHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure LoadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
    procedure killExternalViewer();
    procedure setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);
    function SaveCustomList(AList: TOPPHelpMapList; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
    function RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;

    function AddShortcutMap(AMap: TOPPHelpMap): Integer;
    function SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    procedure NewMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);
  end;

  TOPPHelpShortcutServer = class(TInterfacedObject, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
    fDefaultOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier;
    fLoadPDFIsInProgress: Boolean;
    fSelectedFilename_hash: String;

    procedure openInternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    procedure openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    function sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
    /// <remarks> copyright https://stackoverflow.com/a/12949757 </remarks>
    function ForceForegroundWindow(hwnd: THandle): Boolean;
    procedure SetLoadPDFIsInProgress(const Value: Boolean);
    property SelectedFilename_hash: String read fSelectedFilename_hash write fSelectedFilename_hash;
  public
    procedure LoadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
    function exportControl(AControl: TControl): Boolean;
    procedure killExternalViewer();
    procedure setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);

    procedure ShowHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure ShowHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;

    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);

    function RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;

    function AddShortcutMap(AMap: TOPPHelpMap): Integer;
    function SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    function SaveCustomList(AList: TOPPHelpMapList; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;

    procedure NewMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);

    constructor Create();
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
    property LoadPDFIsInProgress: Boolean read fLoadPDFIsInProgress write SetLoadPDFIsInProgress default false;

  end;

function helpShortcutServer: IOPPHelpShortcutServer;

implementation

uses
  OPP.Help.System.AppExecutor,
  OPP.Help.System.Str,
  OPP.Help.Log,
  OPP.Help.System.Files,
  OPP.Help.Map.Parser.JSON,
  System.TypInfo;

resourcestring
  SWarningMappingIsNotDefinedTemplate = 'Mapping is not defined for [%s]';
  SErrorViewerIsNotSupportingTFormClass = 'viewer is not supporting TFormClass';
  SErrorFindMapCompletionIsNotDefined = 'FindMap completion is not defined';
  SEventRemovedRecordTemplate = 'Removed record: [%s]';
  SEventDidSavedMapsInTemplate = 'Did saved maps in %s';
  SEventSentMessageResultTemplate = 'Sent message result: %s';
  SErrorViewerWasNotCreated = 'viewer was not created';
  SErrorViewerIsNotSupportingIOPPHelpSho = 'viewer is not supporting IOPPHelpShortcutViewer';
  SEventWillShowHelpForTemplate = 'Will show help for %s::%s';
  SErrorGetIdentifierIsNotDefined = 'shortcutserver - onGetIdentifier is not defined in global';
  SEventCreatedShortcutMapTemplate = 'Created shortcut map: %s';
  SErrorCompletionIsNotDefined = 'TOPPHelpShortcutServer.NewMap completion was not defined';
  SEventLoadDataFromFileTemplate = 'Load data from file: "%s"';
  SErrorFileNotFoundTemplate = 'File not found: "%s"';

const
  kContext = 'OPPHelpShortcutServer';
  kPreviewFormClassName: String = 'TOPPHelpPreviewForm';
  kPreviewFormProcessName: String = 'OPPHelpPreview.exe';
  kShortcutMappingDefaultFileName: String = '.\Документация\help.idx';

constructor TOPPHelpShortcutServer.Create();
begin
  inherited Create();

  fPDFMemoryStream := TDictionary<String, TMemoryStream>.Create;

  fShortcutDataset := TOPPHelpShortcutDataset.Create;
  fShortcutDataset.load(TOPPHelpSystemFilesHelper.AbsolutePath(kShortcutMappingDefaultFileName));
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fShortcutDataset.Free;
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

function TOPPHelpShortcutServer.RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;
begin

  try
    try
      fShortcutDataset.RemoveMap(AIdentifier);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally
    result := self.SaveMaps('', callback);
  end;

end;

procedure TOPPHelpShortcutServer.FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
var
  fMap: TOPPHelpMap;
begin
  if not Assigned(completion) then
  begin
    eventLogger.Error(SErrorFindMapCompletionIsNotDefined, kContext);
    exit;
  end;

  fMap := fShortcutDataset.GetMapping(AIdentifier);
  completion(fMap);
end;

procedure TOPPHelpShortcutServer.LoadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
var
  fStream: TMemoryStream;
  fFileName_hash: String;
  fIsAlreadyLoaded: Boolean;
  fStatus: TOPPHelpShortcutServerLoadStreamStatus;
begin
  if not FileExists(AFileName) then
  begin
    eventLogger.Error(Format(SErrorFileNotFoundTemplate, [AFileName]), kContext);
    if Assigned(completion) then
      completion(nil, ssError);
    exit;
  end;

  if LoadPDFIsInProgress then
  begin
    eventLogger.Error('Trying to load second pdf', kContext);
    if Assigned(completion) then
      completion(fStream, ssReused);
    exit;
  end;
  LoadPDFIsInProgress := true;

  eventLogger.Flow(Format(SEventLoadDataFromFileTemplate, [AFileName]), kContext);

  fFileName_hash := AFileName.hashString;
  fPDFMemoryStream.TryGetValue(fFileName_hash, fStream);
  if Assigned(fStream) then
  begin

    fIsAlreadyLoaded := AnsiCompareStr(fFileName_hash, fSelectedFilename_hash) = 0;
    if fIsAlreadyLoaded then
      fStatus := ssReused
    else
      fStatus := ssCreated;

    fSelectedFilename_hash := fFileName_hash;

    LoadPDFIsInProgress := false;
    if Assigned(completion) then
      completion(fStream, fStatus);
    exit;
  end;

  fSelectedFilename_hash := fFileName_hash;

  fStream := TMemoryStream.Create;
  try
    fStream.loadFromFile(AFileName);
    fStream.Position := 0;
    fPDFMemoryStream.Add(fSelectedFilename_hash, fStream);
  finally
    LoadPDFIsInProgress := false;
    if Assigned(completion) then
      completion(fStream, ssCreated);
  end;

end;

procedure TOPPHelpShortcutServer.NewMap(newGUID: TGUID; onApplyDefaults: TOPPHelpMapApplyDefaultsCompletion; completion: TOPPHelpMapCompletion);
var
  fHelpMap: TOPPHelpMap;
  fID: String;
begin
  if not Assigned(completion) then
  begin
    eventLogger.Error(SErrorCompletionIsNotDefined);
    exit;
  end;

  fID := GUIDToString(newGUID);
  eventLogger.Flow(Format(SEventCreatedShortcutMapTemplate, [fID]), kContext);
  fHelpMap := TOPPHelpMap.Create(fID);
  try
    if Assigned(onApplyDefaults) then
      onApplyDefaults(@fHelpMap);
    fShortcutDataset.AddMap(fHelpMap);
    completion(fHelpMap);
  finally
    // fHelpMap.Free;
  end;
end;

procedure TOPPHelpShortcutServer.ShowHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion);
var
  fMapping: TOPPHelpMap;
  fOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier;
  fShortcutIdentifier: String;
  fPredicate: TOPPHelpPredicate;
begin

  fOnGetIdentifier := fDefaultOnGetIdentifier;

  if not Assigned(fOnGetIdentifier) then
  begin
    eventLogger.Error(SErrorGetIdentifierIsNotDefined, kContext);
    exit;
  end;

  eventLogger.Flow(Format(SEventWillShowHelpForTemplate, [ARequest.ActiveControl.classname, ARequest.ActiveControl.name]), kContext);

  fShortcutIdentifier := fOnGetIdentifier(ARequest.ActiveControl);

  fPredicate := nil;
  fMapping := fShortcutDataset.GetMapping(fShortcutIdentifier);
  if Assigned(fMapping) then
  begin
    fPredicate := fMapping.Predicate;
  end;

  if not Assigned(fPredicate) then
  begin
    fPredicate := TOPPHelpPredicate.DefaultPredicate;
  end;

  ShowHelp(fPredicate, viewMode, completion);
end;

function TOPPHelpShortcutServer.exportControl(AControl: TControl): Boolean;
begin
  result := true;
end;

procedure TOPPHelpShortcutServer.openInternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
var
  clazzType: TClass;
  fViewer: TForm;
  result: Boolean;
  Error: Exception;
begin
  result := Assigned(APredicate);
  if not result then
  begin
    if Assigned(completion) then
    begin
      Error := Exception.Create('Predicate is not assigned');
      try
        completion(Error);
      finally
        Error.Free;
      end;
    end;
    exit;
  end;

  clazzType := GetClass(kPreviewFormClassName); // ->TOPPHelpViewFullScreen
  if not clazzType.InheritsFrom(TForm) then
  begin
    eventLogger.Error(SErrorViewerIsNotSupportingTFormClass, kContext);
    exit;
  end;

  if not Supports(clazzType, IOPPHelpShortcutViewer) then
  begin
    eventLogger.Error(SErrorViewerIsNotSupportingIOPPHelpSho, kContext);
    exit;
  end;

  fViewer := TFormClass(clazzType).Create(nil);
  if not Assigned(fViewer) then
  begin
    eventLogger.Error(SErrorViewerWasNotCreated, kContext);
    exit;
  end;

  try
    (fViewer as IOPPHelpShortcutViewer).RunPredicate(APredicate, procedure()begin if Assigned(completion) then completion(nil); end);
    (fViewer as IOPPHelpShortcutViewer).PresentModal;
  finally
    FreeAndNil(fViewer);
  end;

end;

procedure TOPPHelpShortcutServer.openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
begin
  if not Assigned(APredicate) then
  begin
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  TOPPHelpSystemAppExecutor.Execute(kPreviewFormProcessName,
    procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType)
    var
      hwnd: THandle;
      sentResult: TOPPMessagePipeSendResult;
      Error: Exception;
    begin
      if not(Assigned(AList) and (AList.Count <> 0)) then
      begin
        if Assigned(completion) then
        begin
          Error := Exception.Create('Help app was not executed');
          try
            completion(Error);
          finally
            Error.Free;
          end;
        end;
        exit;
      end;

      for hwnd in AList do
      begin
        ForceForegroundWindow(hwnd);
        sentResult := sendOpenPage(hwnd, APredicate);
        eventLogger.Flow(Format(SEventSentMessageResultTemplate, [sentResult.asString]), kContext);
      end;

      if Assigned(completion) then
        completion(nil);
    end);
end;

function TOPPHelpShortcutServer.sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
begin
  result := psrFail;
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
        Predicate.WriteToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;

function TOPPHelpShortcutServer.SaveCustomList(AList: TOPPHelpMapList; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
begin
  AList.Pack;
  result := TOPPHelpMapRESTParser.saveJSON(AList, AFileName, callback);
  if result = 0 then
  begin
    fShortcutDataset.load(AFileName);
  end;
end;

function TOPPHelpShortcutServer.SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  fFileName: String;
  fFileNameFullPath: String;
  fList: TOPPHelpMapList;
begin
  if Length(AFileName) = 0 then
    fFileName := kShortcutMappingDefaultFileName
  else
    fFileName := AFileName;

  eventLogger.Flow(Format(SEventDidSavedMapsInTemplate, [fFileName]), kContext);

  fFileNameFullPath := TOPPHelpSystemFilesHelper.AbsolutePath(fFileName);

  fList := fShortcutDataset.List();
  try
    result := SaveCustomList(fList, fFileNameFullPath, callback);
  finally
    fList.Clear;
    fList.Free;
  end;
end;

procedure TOPPHelpShortcutServer.setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);
begin
  fDefaultOnGetIdentifier := AOnGetIdentifier;
end;

procedure TOPPHelpShortcutServer.SetLoadPDFIsInProgress(const Value: Boolean);
begin
  fLoadPDFIsInProgress := Value;
end;

procedure TOPPHelpShortcutServer.ShowHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion);
begin
  case viewMode of
    vmInternal:
      openInternalViewer(APredicate, completion);
    vmExternal:
      openExternalViewer(APredicate, completion);
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
      result := false;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadProcessID(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, true) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, false);
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
  TOPPSystemMessageHelper.KillProcess(kPreviewFormProcessName);
end;

function TOPPHelpShortcutServer.AddShortcutMap(AMap: TOPPHelpMap): Integer;
begin
  result := fShortcutDataset.AddMap(AMap);
end;

// ---

var
  fLock: TCriticalSection;
  fHelpServer: IOPPHelpShortcutServer;

function helpShortcutServer: IOPPHelpShortcutServer;
begin
  fLock.Acquire;
  try
    if not Assigned(fHelpServer) then
    begin
      fHelpServer := TOPPHelpShortcutServer.Create();
    end;
    result := fHelpServer;
  finally
    fLock.Release;
  end;
end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
