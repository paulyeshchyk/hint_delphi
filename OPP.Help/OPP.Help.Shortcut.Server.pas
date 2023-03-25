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
  OPP.Help.PreviewForm;

type
  TOPPHelpViewMode = (vmInternal, vmExternal);
  TPreviewFormClass = class of TOPPHelpPreviewForm;

  IOPPHelpShortcutServer = interface
    function exportControl(AControl: TControl): Boolean;
    function showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode): Boolean; overload;
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean; overload;
    function loadPDF(AFileName: String): TMemoryStream;
    procedure killExternalViewer();
  end;

  TOPPHelpShortcutServer = class(TComponent, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
    function openInternalViewer(APredicate: TOPPHelpPredicate): Boolean;
    function openExternalViewer(APredicate: TOPPHelpPredicate): Boolean;
    procedure sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate);
  public
    function loadPDF(AFileName: String): TMemoryStream;
    function exportControl(AControl: TControl): Boolean;
    function showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode): Boolean; overload;
    function showHelp(ARequest: TOPPHelpShortcutRequest): Boolean; overload;
    procedure killExternalViewer();

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

procedure Register;

implementation

uses
  OPP.Help.System.Str,
  OPP.Help.System.Messaging.Pipe;

const
  shortcutJSONFileName: String = 'help\mapping\shortcut_matrix.json';

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

function TOPPHelpShortcutServer.showHelp(APredicate: TOPPHelpPredicate; viewMode: TOPPHelpViewMode): Boolean;
begin
  case viewMode of
    vmInternal:
      result := openInternalViewer(APredicate);
    vmExternal:
      result := openExternalViewer(APredicate);
  else
    result := false;
  end;
end;

function TOPPHelpShortcutServer.showHelp(ARequest: TOPPHelpShortcutRequest): Boolean;
var
  fMapping: TOPPHelpShortcutMap;
begin
  fMapping := fShortcutDataset.GetMapping(ARequest.shortcutIdentifier);
  result := Assigned(fMapping);
  if not result then
    exit;

  result := showHelp(fMapping.Predicate, vmInternal);
end;

function TOPPHelpShortcutServer.exportControl(AControl: TControl): Boolean;
begin
  result := true;
end;

function TOPPHelpShortcutServer.openInternalViewer(APredicate: TOPPHelpPredicate): Boolean;
var
  fPreviewForm: TOPPHelpPreviewForm;
begin
  result := Assigned(APredicate);
  if not result then
    exit;

  fPreviewForm := TOPPHelpPreviewForm.Create(nil);
  try
    fPreviewForm.runPredicate(APredicate);
    fPreviewForm.showModal;
  finally
    FreeAndNil(fPreviewForm);
  end;
end;

function TOPPHelpShortcutServer.openExternalViewer(APredicate: TOPPHelpPredicate): Boolean;
var
  fWindowClassHandleList: TList<THandle>;
  hwnd: THandle;
  fSelfHandle: THandle;
  fOPPViewerClassName: String;

const
  OPPViewerProcessName: String = 'OPPHelpPreview.exe';
begin

  result := false;

  fOPPViewerClassName := TPreviewFormClass.classname;

  fSelfHandle := Application.Handle;

  fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
  if Assigned(fWindowClassHandleList) then
  begin
    if fWindowClassHandleList.Count = 0 then
    begin
      result := TOPPSystemMessageHelper.RunProcess(OPPViewerProcessName, fSelfHandle, 300,
        procedure()
        var
          hwnd: THandle;
        begin
          fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
          if Assigned(fWindowClassHandleList) then
          begin
            for hwnd in fWindowClassHandleList do
            begin
              sendOpenPage(hwnd, APredicate);
            end;
          end;
        end);

    end else begin
      for hwnd in fWindowClassHandleList do
      begin
        sendOpenPage(hwnd, APredicate);
      end;
    end;
  end else begin
    result := TOPPSystemMessageHelper.RunProcess(OPPViewerProcessName, fSelfHandle, 300,
      procedure()
      begin
        sendOpenPage(hwnd, APredicate);
      end);
  end;
end;

procedure TOPPHelpShortcutServer.sendOpenPage(AProcessHandle: THandle; Predicate: TOPPHelpPredicate);
var
  fMessagePipe: TOPPMessagePipe;
  fSelfHandle: THandle;
begin
  if AProcessHandle = 0 then
  begin
    // ShowMessage('Невозможно запустить окно помощи.');
    exit;
  end;

  fSelfHandle := Application.Handle; // self.Handle
  fMessagePipe := TOPPMessagePipe.Create;

  fMessagePipe.SendRecord(AProcessHandle, fSelfHandle, '',
    procedure(AStream: TStream)
    begin
      Predicate.writeToStream(AStream);
    end);

  fMessagePipe.Free;
end;

procedure TOPPHelpShortcutServer.killExternalViewer();
begin
  // TOPPSystemMessageHelper.KillProcess(OPPViewerProcessName);
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
