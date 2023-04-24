unit OPP.Keyboard.Shortcut.Manager;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  WinAPI.Windows,
  System.SysUtils,
  vcl.forms;

type
  THookCallback = reference to procedure();

  IOPPKeyboardShortcutManager = interface
    procedure registerHook(AShortcut: TShortcut; callback: THookCallback);
    procedure unregisterHook(AShortcut: TShortcut);
    procedure replaceHookShortcut(AInitialShortcut, AFinalShortcut: TShortcut);
    procedure run(AShortcut: TShortcut);
  end;

  TOPPKeyboardShortcutManager = class(TInterfacedObject, IOPPKeyboardShortcutManager)
  private
    fHooks: TDictionary<TShortcut, THookCallback>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure registerHook(AShortcut: TShortcut; callback: THookCallback);
    procedure unregisterHook(AShortcut: TShortcut);
    procedure replaceHookShortcut(AInitialShortcut, AFinalShortcut: TShortcut);
    procedure run(AShortcut: TShortcut);
  end;

function keyboardShortcutManager: IOPPKeyboardShortcutManager;

implementation

uses
  OPP.Help.Log,
  OPP.Help.System.Messaging;

const
  kContext = 'TOPPKeyboardHook';

  { TOPPKeyboardHook }

constructor TOPPKeyboardShortcutManager.Create;
begin
  fHooks := TDictionary<TShortcut, THookCallback>.Create;
end;

destructor TOPPKeyboardShortcutManager.Destroy;
begin
  fHooks.Clear;
  fHooks.Free;
  inherited;
end;

procedure TOPPKeyboardShortcutManager.registerHook(AShortcut: TShortcut; callback: THookCallback);
begin
  if AShortcut <= 0 then
  begin
    eventLogger.Warning('Shortcut is equal to zero', kContext);
    exit;
  end;
  fHooks.Add(AShortcut, callback);
end;

procedure TOPPKeyboardShortcutManager.replaceHookShortcut(AInitialShortcut, AFinalShortcut: TShortcut);
var
  fCallback: THookCallback;
begin
  if AInitialShortcut = AFinalShortcut then
  begin
    eventLogger.Debug('Nothing to replace', kContext);
    exit;
  end;

  try
    fHooks.TryGetValue(AInitialShortcut, fCallback);
    if Assigned(fCallback) then
    begin
      self.unregisterHook(AInitialShortcut);
      self.registerHook(AFinalShortcut, fCallback);
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPKeyboardShortcutManager.run(AShortcut: TShortcut);
var
  callback: THookCallback;
begin
  try
    fHooks.TryGetValue(AShortcut, callback);
    if Assigned(callback) then begin
      eventLogger.Flow(Format('executed shortcut %d', [AShortcut]), kContext);
      callback();
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, kContext);
    end;
  end;
end;

procedure TOPPKeyboardShortcutManager.unregisterHook(AShortcut: TShortcut);
begin
  fHooks.Remove(AShortcut);
end;

var
  fKeyboardHookLock: TCriticalSection;
  fKeyboardShortcutManager: IOPPKeyboardShortcutManager;

function keyboardShortcutManager: IOPPKeyboardShortcutManager;
begin
  fKeyboardHookLock.Acquire;
  try
    if not Assigned(fKeyboardShortcutManager) then
    begin
      fKeyboardShortcutManager := TOPPKeyboardShortcutManager.Create();
    end;
    Result := fKeyboardShortcutManager;
  finally
    fKeyboardHookLock.Release;
  end;

end;

initialization

fKeyboardHookLock := TCriticalSection.Create;

finalization

fKeyboardHookLock.Free;

end.
