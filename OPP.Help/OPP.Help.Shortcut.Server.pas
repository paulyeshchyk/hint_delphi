﻿unit OPP.Help.Shortcut.Server;

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

  TOPPHelpShortcutPresentingResult = (prSuccess, prFail);
  TOPPHelpShortcutPresentingCompletion = reference to procedure(APresentingResult: TOPPHelpShortcutPresentingResult);
  TOPPHelpShortcutOnGetIdentifier = reference to function(AControl: TControl): String;
  TOPPHelpShortcutServerLoadStreamStatus = (ssCreated, ssReused, ssError);
  TOPPHelpShortcutServerLoadStreamCompletion = reference to procedure(AStream: TMemoryStream; AStatus: TOPPHelpShortcutServerLoadStreamStatus);

  IOPPHelpShortcutServer = interface
    function exportControl(AControl: TControl): Boolean;
    procedure showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure loadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
    procedure killExternalViewer();
    procedure setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);
    function SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
    function RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;

    function AddShortcutMap(AMap: TOPPHelpMap): Integer;
    function SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    procedure NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);
  end;

  TOPPHelpShortcutServer = class(TComponent, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
    fDefaultOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier;

    procedure openInternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    procedure openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    function sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
    /// <remarks> copyright https://stackoverflow.com/a/12949757 </remarks>
    function ForceForegroundWindow(hwnd: THandle): Boolean;
  public
    procedure loadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
    function exportControl(AControl: TControl): Boolean;
    procedure killExternalViewer();
    procedure setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);

    procedure showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);

    function RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;

    function AddShortcutMap(AMap: TOPPHelpMap): Integer;
    function SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
    function SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;

    procedure NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);

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
  OPP.Help.System.Files,
  OPP.Help.Map.Parser.JSON,
  System.TypInfo;

resourcestring
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
  kEventFlowName = 'Shortcut server';

const
  kPreviewFormClassName: String = 'TOPPHelpPreviewForm';
  kPreviewFormProcessName: String = 'OPPHelpPreview.exe';
  kShortcutMappingDefaultFileName: String = '.\help\mapping\shortcut_matrix.json';

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
  fShortcutDataset.load(kShortcutMappingDefaultFileName);
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fShortcutDataset.Free;
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

function TOPPHelpShortcutServer.RemoveHelpMap(AIdentifier: TOPPHelpMetaIdentifierType; callback: TOPPHelpErrorCompletion): Integer;
var
  fMap: TOPPHelpMap;
  itemsToRemove: TList<TOPPHelpMap>;
begin

  itemsToRemove := TList<TOPPHelpMap>.Create();
  try
    for fMap in self.fShortcutDataset.list do
    begin
      if fMap = nil then
        continue;
      if fMap.ComponentIdentifier = AIdentifier then
        itemsToRemove.Add(fMap);
    end;

    for fMap in itemsToRemove do
    begin
      if fMap = nil then
        continue;
      eventLogger.Flow(Format(SEventRemovedRecordTemplate, [fMap.Identifier]), kEventFlowName);
      fShortcutDataset.list.Remove(fMap);
    end;

  finally
    itemsToRemove.Free;
    result := self.SaveMaps('', callback);
  end;

end;

procedure TOPPHelpShortcutServer.FindHelpMap(const AIdentifier: TOPPHelpMetaIdentifierType; completion: TOPPHelpMapCompletion);
var
  fMap: TOPPHelpMap;
  result: TOPPHelpMap;
begin
  result := nil;
  if not Assigned(completion) then
  begin
    eventLogger.Error(SErrorFindMapCompletionIsNotDefined,kEventFlowName);
    exit;
  end;

  for fMap in fShortcutDataset.list do
  begin
    if fMap = nil then
      continue;
    if not Assigned(fMap) then
      continue;
    if fMap.ComponentIdentifier = AIdentifier then
    begin
      result := fMap;
      break;
    end;
  end;

  completion(result);
end;

procedure TOPPHelpShortcutServer.loadPDF(AFileName: String; completion: TOPPHelpShortcutServerLoadStreamCompletion);
var
  fFileNameHash: String;
  fStream: TMemoryStream;
begin
  if not FileExists(AFileName) then
  begin
    eventLogger.Error(Format(SErrorFileNotFoundTemplate, [AFileName]), kEventFlowName);
    if Assigned(completion) then
      completion(nil, ssError);
    exit;
  end;

  eventLogger.Flow(Format(SEventLoadDataFromFileTemplate, [AFileName]), kEventFlowName);

  fFileNameHash := AFileName.hashString;
  fPDFMemoryStream.TryGetValue(fFileNameHash, fStream);
  if Assigned(fStream) then
  begin
    if Assigned(completion) then
      completion(fStream, ssReused);
  end else begin

    fStream := TMemoryStream.Create;
    try
      fStream.loadFromFile(AFileName);
      fStream.Position := 0;
      fPDFMemoryStream.Add(fFileNameHash, fStream);
    finally
      if Assigned(completion) then
        completion(fStream, ssCreated);
    end;
  end;
end;

procedure TOPPHelpShortcutServer.NewMap(newGUID: TGUID; completion: TOPPHelpMapCompletion);
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
  eventLogger.Flow(Format(SEventCreatedShortcutMapTemplate, [fID]), kEventFlowName);
  fHelpMap := TOPPHelpMap.Create(fID);
  try
    fShortcutDataset.AddMap(fHelpMap);
    completion(fHelpMap);
  finally
    // fHelpMap.Free;
  end;
end;

procedure TOPPHelpShortcutServer.showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion);
begin
  case viewMode of
    vmInternal:
      openInternalViewer(APredicate, completion);
    vmExternal:
      openExternalViewer(APredicate, completion);
  end;
end;

procedure TOPPHelpShortcutServer.showHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion);
var
  fMapping: TOPPHelpMap;
  fOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier;
  fShortcutIdentifier: String;
begin

  fOnGetIdentifier := fDefaultOnGetIdentifier;

  if not Assigned(fOnGetIdentifier) then
  begin
    eventLogger.Error(SErrorGetIdentifierIsNotDefined, kEventFlowName);
    exit;
  end;

  eventLogger.Flow(Format(SEventWillShowHelpForTemplate, [ARequest.ActiveControl.classname, ARequest.ActiveControl.name]), 'OPPHelpShortcutServer');

  fShortcutIdentifier := fOnGetIdentifier(ARequest.ActiveControl);

  fMapping := fShortcutDataset.GetMapping(fShortcutIdentifier);
  if not Assigned(fMapping) then
  begin
    if Assigned(completion) then
      completion(prFail);
    exit;
  end;

  showHelp(fMapping.Predicate, viewMode, completion);
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
begin
  result := Assigned(APredicate);
  if not result then
  begin
    if Assigned(completion) then
      completion(prFail);
    exit;
  end else begin
    clazzType := GetClass(kPreviewFormClassName); // ->TOPPHelpViewFullScreen
    { clazzType := GetTypeData(AViewerClassInfo).ClassType };
    if not clazzType.InheritsFrom(TForm) then
    begin
      eventLogger.Error(SErrorViewerIsNotSupportingTFormClass, kEventFlowName);
      exit;
    end;

    if not Supports(clazzType, IOPPHelpShortcutViewer) then
    begin
      eventLogger.Error(SErrorViewerIsNotSupportingIOPPHelpSho,kEventFlowName);
      exit;
    end;

    fViewer := TFormClass(clazzType).Create(nil);
    if not Assigned(fViewer) then
    begin
      eventLogger.Error(SErrorViewerWasNotCreated, kEventFlowName);
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

procedure TOPPHelpShortcutServer.openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
begin
  TOPPHelpSystemAppExecutor.Execute(kPreviewFormProcessName,
    procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType)
    var
      hwnd: THandle;
      sentResult: TOPPMessagePipeSendResult;
    begin
      if not(Assigned(AList) and (AList.Count <> 0)) then
      begin
        if Assigned(completion) then
          completion(prFail);
      end else begin

        for hwnd in AList do
        begin
          ForceForegroundWindow(hwnd);
          sentResult := SendOpenPage(hwnd, APredicate);
          eventLogger.Flow(Format(SEventSentMessageResultTemplate,[sentResult.asString]),kEventFlowName);
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
        Predicate.writeToStream(AStream);
      end);
  finally
    fMessagePipe.Free;
  end;
end;

function TOPPHelpShortcutServer.SaveCustomList(AList: TList<TOPPHelpMap>; AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
begin
  result := TOPPHelpMapRESTParser.saveJSON(AList, AFileName, callback);
end;

function TOPPHelpShortcutServer.SaveMaps(AFileName: String; callback: TOPPHelpErrorCompletion): Integer;
var
  fFileName: String;
  fFileNameFullPath: String;
begin
  if Length(AFileName) = 0 then
    fFileName := kShortcutMappingDefaultFileName
  else
    fFileName := AFileName;

  eventLogger.Flow(Format(SEventDidSavedMapsInTemplate, [fFileName]), kEventFlowName);

  fFileNameFullPath := TOPPHelpSystemFilesHelper.AbsolutePath(fFileName);

  result := SaveCustomList(fShortcutDataset.list, fFileNameFullPath, callback);
end;

procedure TOPPHelpShortcutServer.setDefaultOnGetIdentifier(AOnGetIdentifier: TOPPHelpShortcutOnGetIdentifier);
begin
  fDefaultOnGetIdentifier := AOnGetIdentifier;
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
  TOPPSystemMessageHelper.KillProcess(kPreviewFormProcessName);
end;

function TOPPHelpShortcutServer.AddShortcutMap(AMap: TOPPHelpMap): Integer;
begin
  result := fShortcutDataset.AddMap(AMap);
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
