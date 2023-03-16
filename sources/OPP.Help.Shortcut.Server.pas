unit OPP.Help.Shortcut.Server;

interface

uses
  System.SyncObjs, System.Generics.Collections, System.Classes,
  Vcl.Controls, Vcl.Forms,
  OPP.Help.ShortcutMapping;

type
  IOPPHelpShortcutServer = interface
    function showHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    function showManual(pageIndex: Integer): Boolean;
  end;

  TOPPHelpShortcutServer = class(TInterfacedObject, IOPPHelpShortcutServer)
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpMap>;
    fPDFMemoryStream: TMemoryStream;
    procedure loadMapping(AFileName: String);
    procedure loadPDF(AFileName: String);
  public
    function showHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    function showManual(pageIndex: Integer): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;

implementation

uses
  OPP.Help.LargeForm;

const
  shortcutJSONFileName: String = 'help\shortcut_matrix.json';
  pdfFileName: String          = 'docs\readme.pdf';

var
  fLock: TCriticalSection;
  fHelpServer: IOPPHelpShortcutServer;

function helpShortcutServer: IOPPHelpShortcutServer;
begin
  fLock.Acquire;
  try
    if not Assigned(fHelpServer) then begin
      fHelpServer := TOPPHelpShortcutServer.Create;
    end;
    result := fHelpServer;
  finally
    fLock.Release;
  end;
end;

{ }

constructor TOPPHelpShortcutServer.Create;
begin
  inherited Create;

  loadMapping(shortcutJSONFileName);
  loadPDF(pdfFileName);
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

procedure TOPPHelpShortcutServer.loadMapping(AFileName: String);
var
  shortcut_matrix: TList<TOPPHelpMap>;
  map: TOPPHelpMap;
begin
  shortcut_matrix := TOPPHelpMap.readJSON(AFileName);
  fShortcutHelpMatrix := TDictionary<String, TOPPHelpMap>.Create();
  for map in shortcut_matrix do begin
    fShortcutHelpMatrix.add(map.HelpKeyword, map);
  end;
  shortcut_matrix.Free;
end;

procedure TOPPHelpShortcutServer.loadPDF(AFileName: String);
begin
  fPDFMemoryStream := TMemoryStream.Create;
  fPDFMemoryStream.loadFromFile(AFileName);
  fPDFMemoryStream.Position := 0;
end;

function TOPPHelpShortcutServer.showManual(pageIndex: Integer): Boolean;
var
  helpForm: TOPPHelpLargeForm;
begin
  helpForm := TOPPHelpLargeForm.Create(nil);
  helpForm.stream := fPDFMemoryStream;
  helpForm.openPage(3);
  helpForm.ShowModal;
  result := true;
end;

function TOPPHelpShortcutServer.showHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
var
  helpForm: TOPPHelpLargeForm;
  helpData: String;
  mapping: TOPPHelpMap;
begin
  helpData := String(Data);
  try
    fShortcutHelpMatrix.TryGetValue(helpData, mapping);
  finally
    if Assigned(mapping) then begin
      helpForm := TOPPHelpLargeForm.Create(nil);
      helpForm.stream := fPDFMemoryStream;
      helpForm.map := mapping;
      helpForm.ShowModal;
    end else begin
      // ShowMessage('Help not found');
    end;
  end;

  CallHelp := false;
  result := true;
end;

initialization

fLock := TCriticalSection.Create;

finalization

fLock.Free;

end.
