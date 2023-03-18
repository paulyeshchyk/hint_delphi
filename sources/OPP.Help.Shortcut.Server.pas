unit OPP.Help.Shortcut.Server;

interface

uses
  System.SysUtils, System.SyncObjs, System.Generics.Collections, System.Classes,
  System.Messaging,
  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.Forms,
  OPP.Help.Shortcut.Dataset,
  OPP.Help.Shortcut.Request,
  OPP.System, OPP.Help.Shortcut.Mapping;

type
  IOPPHelpShortcutServer = interface
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean;
    function showManual(pageIndex: Integer): Boolean;
  end;

  TOPPHelpShortcutServer = class(TInterfacedObject, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TMemoryStream;
    procedure loadPDF(AFileName: String);
  public
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean;
    function showManual(pageIndex: Integer): Boolean;
    constructor create;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

implementation

uses
  OPP.Help.LargeForm;

const
  shortcutJSONFileName: String = 'help\shortcut_matrix.json';
  pdfFileName: String = 'docs\readme.pdf';

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

  fShortcutDataset := TOPPHelpShortcutDataset.create;
  fShortcutDataset.load(shortcutJSONFileName);
  loadPDF(pdfFileName);
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fShortcutDataset.Free;
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

procedure TOPPHelpShortcutServer.loadPDF(AFileName: String);
begin
  fPDFMemoryStream := TMemoryStream.create;
  fPDFMemoryStream.loadFromFile(AFileName);
  fPDFMemoryStream.Position := 0;
end;

function TOPPHelpShortcutServer.showManual(pageIndex: Integer): Boolean;
var
  helpForm: TOPPHelpLargeForm;
begin
  helpForm := TOPPHelpLargeForm.create(nil);
  helpForm.stream := fPDFMemoryStream;
  helpForm.openPage(3);
  helpForm.ShowModal;
  result := true;
end;

function TOPPHelpShortcutServer.showHelp(Request: TOPPHelpShortcutRequest): Boolean;
var
  helpForm: TOPPHelpLargeForm;
  helpData: String;
  Mapping: TOPPHelpMap;
begin
  helpData := Request.getHelpData();
  Mapping := fShortcutDataset.getMapping(helpData);
  if Assigned(Mapping) then
  begin
    helpForm := TOPPHelpLargeForm.create(nil);
    helpForm.stream := fPDFMemoryStream;
    helpForm.map := Mapping;
    helpForm.ShowModal;
    result := true;
  end else begin
    result := false;
  end;
end;

initialization

fLock := TCriticalSection.create;

finalization

fLock.Free;

end.
