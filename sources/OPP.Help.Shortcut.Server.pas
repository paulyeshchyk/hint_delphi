unit OPP.Help.Shortcut.Server;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.SyncObjs, System.Classes,

  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.Forms,
  OPP.Help.Shortcut.Dataset,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Mapping;

type
  IOPPHelpShortcutServer = interface
    function showHelp(AMapping: TOPPHelpShortcutMap): Boolean;overload;
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean;overload;
    function showManual(pageIndex: Integer): Boolean;
  end;

  TOPPHelpShortcutServer = class(TInterfacedObject, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
    function loadPDF(AFileName: String): TMemoryStream;
  public
    function showHelp(AMapping: TOPPHelpShortcutMap): Boolean;overload;
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean;overload;
    function showManual(pageIndex: Integer): Boolean;
    constructor create;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

implementation

uses
  OPP.Help.LargeForm, OPP.Help.System.Str;

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
      fHelpServer := TOPPHelpShortcutServer.create;
    end;
    result := fHelpServer;
  finally
    fLock.Release;
  end;
end;

// ---

constructor TOPPHelpShortcutServer.create;
begin
  inherited create;

  fPDFMemoryStream := TDictionary<String, TMemoryStream>.create;

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
  hash: String;
  fStream: TMemoryStream;
begin
  hash := AFileName.hashString;
  fPDFMemoryStream.TryGetValue(hash, fStream);
  if not Assigned(fStream) then
  begin
    fStream := TMemoryStream.create;
    fStream.loadFromFile(AFileName);
    fStream.Position := 0;

    fPDFMemoryStream.Add(hash, fStream);
  end;
  result := fStream;
end;

function TOPPHelpShortcutServer.showManual(pageIndex: Integer): Boolean;
// var
// helpForm: TOPPHelpLargeForm;
begin
  // helpForm := TOPPHelpLargeForm.create(nil);
  // helpForm.stream := fPDFMemoryStream;
  // helpForm.openPage(3);
  // helpForm.ShowModal;
  // result := true;
end;

function TOPPHelpShortcutServer.showHelp(AMapping: TOPPHelpShortcutMap): Boolean;
var
  fHelpForm: TOPPHelpLargeForm;
  fStream: TMemoryStream;
begin
  if Assigned(AMapping) then
  begin
    fStream := loadPDF(AMapping.predicate.fileName);
    fHelpForm := TOPPHelpLargeForm.create(nil);
    fHelpForm.stream := fStream;
    fHelpForm.shortcutMap := AMapping;
    fHelpForm.ShowModal;
    result := true;
  end else begin
    result := false;
  end;
end;

function TOPPHelpShortcutServer.showHelp(Request: TOPPHelpShortcutRequest): Boolean;
var
  fhelpData: String;
  fMapping: TOPPHelpShortcutMap;
begin
  fhelpData := Request.GetShortcutIdentifier();
  fMapping := fShortcutDataset.getMapping(fhelpData);
  showHelp(fMapping);
end;

initialization

fLock := TCriticalSection.create;

finalization

fLock.Free;

end.
