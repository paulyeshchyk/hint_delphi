﻿unit OPP.Help.Shortcut.Server;

interface

uses

//vcl.dialogs,


  System.Generics.Collections,
  System.SysUtils, System.SyncObjs, System.Classes,

  WinAPI.Messages,
  WinAPI.Windows,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,
  OPP.Help.Predicate,
  OPP.Help.Shortcut.Dataset,
  OPP.Help.Shortcut.Request,
  OPP.Help.Shortcut.Mapping;

type
  IOPPHelpShortcutServer = interface
    function exportControl(AControl: TControl): Boolean;
    function showHelp(APredicate: TOPPHelpPredicate): Boolean; overload;
    function showHelp(Request: TOPPHelpShortcutRequest): Boolean; overload;
    function loadPDF(AFileName: String): TMemoryStream;
  end;

  TOPPHelpShortcutServer = class(TComponent, IOPPHelpShortcutServer)
  private
    fShortcutDataset: TOPPHelpShortcutDataset;
    fPDFMemoryStream: TDictionary<String, TMemoryStream>;
  public
    function loadPDF(AFileName: String): TMemoryStream;
    function exportControl(AControl: TControl): Boolean;
    function showHelp(APredicate: TOPPHelpPredicate): Boolean; overload;
    function showHelp(ARequest: TOPPHelpShortcutRequest): Boolean; overload;

    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    property ShortcutDataset: TOPPHelpShortcutDataset read fShortcutDataset write fShortcutDataset;
  end;

function helpShortcutServer: IOPPHelpShortcutServer;
procedure Register;

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
      fHelpServer := TOPPHelpShortcutServer.Create(nil);
    end;
    result := fHelpServer;
  finally
    fLock.Release;
  end;
end;

// ---

constructor TOPPHelpShortcutServer.create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fPDFMemoryStream := TDictionary<String, TMemoryStream>.create;

  fShortcutDataset := TOPPHelpShortcutDataset.create;
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
    fStream := TMemoryStream.create;
    fStream.loadFromFile(AFileName);
    fStream.Position := 0;

    fPDFMemoryStream.Add(fFileNameHash, fStream);
  end;
  result := fStream;
end;

function TOPPHelpShortcutServer.showHelp(APredicate: TOPPHelpPredicate): Boolean;
var
  fHelpForm: TOPPHelpLargeForm;
  fStream: TMemoryStream;
begin
  result := Assigned(APredicate);
  if not result then
    exit;

  fStream := loadPDF(APredicate.fileName);
  fHelpForm := TOPPHelpLargeForm.create(nil);
  fHelpForm.stream := fStream;
  fHelpForm.predicate := APredicate;
  fHelpForm.ShowModal;
end;

function TOPPHelpShortcutServer.showHelp(ARequest: TOPPHelpShortcutRequest): Boolean;
var
  fMapping: TOPPHelpShortcutMap;
begin
  fMapping := fShortcutDataset.GetMapping(ARequest.shortcutIdentifier);
  result := Assigned(fMapping);
  if not result then
    exit;

  showHelp(fMapping.predicate);
end;

function TOPPHelpShortcutServer.exportControl(AControl: TControl): Boolean;
begin
  result := true;
end;

procedure Register;
begin
  RegisterComponents('OPPHelp',[TOPPHelpShortcutServer])
end;

initialization

fLock := TCriticalSection.create;

finalization

fLock.Free;

end.
