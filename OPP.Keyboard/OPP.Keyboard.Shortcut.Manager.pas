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
    procedure run(AShortcut: TShortcut);
  end;

  TOPPKeyboardShortcutManager = class(TInterfacedObject, IOPPKeyboardShortcutManager)
  private
    hhk: HHOOK;
    fHooks: TDictionary<TShortcut, THookCallback>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure registerHook(AShortcut: TShortcut; callback: THookCallback);
    procedure unregisterHook(AShortcut: TShortcut);
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
  fHooks.Add(AShortcut, callback);
end;

procedure TOPPKeyboardShortcutManager.run(AShortcut: TShortcut);
var
  callback: THookCallback;
begin
  try
    try
      eventLogger.Debug(Format('shortcut %d',[AShortcut]));
      fHooks.TryGetValue(AShortcut, callback);
      if assigned(callback) then
        callback();
    except
      on E: Exception do
      begin
        eventLogger.Error(E, kContext);
      end;
    end;
  finally

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
    if not assigned(fKeyboardShortcutManager) then
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
