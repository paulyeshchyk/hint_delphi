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
  OPP.Help.System.Messaging.Pipe,
  OPP.Help.PreviewForm;

type
  TOPPHelpViewMode = (vmInternal, vmExternal);
  TPreviewFormClass = class of TOPPHelpPreviewForm;

  TOPPHelpShortcutPresentingResult = (prSuccess, prFail);
  TOPPHelpShortcutPresentingCompletion = reference to procedure(APresentingResult: TOPPHelpShortcutPresentingResult);

  IOPPHelpShortcutServer = interface
    function exportControl(AControl: TControl): Boolean;
    procedure showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    function loadPDF(AFileName: String): TMemoryStream;
    procedure killExternalViewer();
  end;

  TOPPHelpShortcutServer = class(TComponent, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
    procedure openInternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    procedure openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
    function sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate): TOPPMessagePipeSendResult;
  public
    function loadPDF(AFileName: String): TMemoryStream;
    function exportControl(AControl: TControl): Boolean;
    procedure showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure showHelp(ARequest: TOPPHelpShortcutRequest; viewMode: TOPPHelpViewMode; completion: TOPPHelpShortcutPresentingCompletion); overload;
    procedure killExternalViewer();

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

procedure Register;

implementation

uses
  OPP.Help.System.AppExecutor,
  OPP.Help.System.Str;

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
  fMapping: TOPPHelpShortcutMap;
begin
  fMapping := fShortcutDataset.GetMapping(ARequest.shortcutIdentifier);
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
  fPreviewForm: TOPPHelpPreviewForm;
  result: Boolean;
begin
  result := Assigned(APredicate);
  if not result then
  begin
    if Assigned(completion) then
      completion(prFail);
    exit;
  end else begin

    fPreviewForm := TOPPHelpPreviewForm.Create(nil);
    try
      fPreviewForm.runPredicate(APredicate);
      fPreviewForm.showModal;
    finally
      FreeAndNil(fPreviewForm);
    end;
    if Assigned(completion) then
      completion(prSuccess);

  end;
end;

procedure TOPPHelpShortcutServer.openExternalViewer(APredicate: TOPPHelpPredicate; completion: TOPPHelpShortcutPresentingCompletion);
begin
  TOPPHelpSystemAppExecutor.Execute(OPPViewerProcessName, TOPPHelpPreviewForm,
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
          sendOpenPage(hwnd, APredicate);

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
