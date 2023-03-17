unit OPP.Help.Shortcut.Server;

interface

uses
  System.SysUtils, System.SyncObjs, System.Generics.Collections, System.Classes,
  System.Messaging,
  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.Forms,
  OPP.Help.Shortcut.Request,
  OPP.System, OPP.Help.Shortcut.Mapping;

type
  IOPPHelpShortcutServer = interface
    function showHelp(request: TOPPHelpShortcutRequest): Boolean;
    function showManual(pageIndex: Integer): Boolean;
  end;

  TOPPHelpShortcutServer = class(TInterfacedObject, IOPPHelpShortcutServer)
  private
    fShortcutHelpMatrix: TDictionary<String, TOPPHelpMap>;
    fPDFMemoryStream: TMemoryStream;
    procedure loadMapping(AFileName: String);
    procedure loadPDF(AFileName: String);
  public
    function showHelp(request: TOPPHelpShortcutRequest): Boolean;
    function showManual(pageIndex: Integer): Boolean;
    constructor create;
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

  fShortcutHelpMatrix := TDictionary<String, TOPPHelpMap>.create;
  loadMapping(shortcutJSONFileName);
  loadPDF(pdfFileName);
end;

destructor TOPPHelpShortcutServer.Destroy;
begin
  fShortcutHelpMatrix.Free;
  fPDFMemoryStream.Free;
  inherited Destroy;
end;

procedure TOPPHelpShortcutServer.loadMapping(AFileName: String);
var
  callback: TOPPHelpMapJSONReadCallback;
begin
  callback := procedure(AList: TList<TOPPHelpMap>; error: Exception)
    var
      map: TOPPHelpMap;
    begin
      fShortcutHelpMatrix.Clear;
      if Assigned(error) then begin
        OutputDebugString(error.ClassName.toWideChar);
        exit;
      end;
      for map in AList do begin
        self.fShortcutHelpMatrix.add(map.HelpKeyword, map);
      end;
    end;

  TOPPHelpMap.readJSON(AFileName, callback);
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

function TOPPHelpShortcutServer.showHelp(request: TOPPHelpShortcutRequest): Boolean;
var
  helpForm: TOPPHelpLargeForm;
  helpData: String;
  mapping: TOPPHelpMap;
begin
  helpData := request.activeControl.HelpKeyword;
  try
    fShortcutHelpMatrix.TryGetValue(helpData, mapping);
  finally
    if Assigned(mapping) then begin
      helpForm := TOPPHelpLargeForm.create(nil);
      helpForm.stream := fPDFMemoryStream;
      helpForm.map := mapping;
      helpForm.ShowModal;
      result := true;
    end else begin
      result := false;
    end;
  end;
end;

initialization

fLock := TCriticalSection.create;

finalization

fLock.Free;

end.
