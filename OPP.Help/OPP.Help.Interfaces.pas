unit OPP.Help.Interfaces;

interface

uses
  System.Classes, System.SysUtils,
  OPP.Help.Predicate,
  OPP.Help.Hint;

type

  IOPPHelpHintDataReader = interface

    /// <summary>
    /// Загружает файл подсказок
    ///
    /// </summary>
    function loadData(AFileName: String): TOPPHelpHintServerLoadResultType;

    function FindHintDataForBookmarkIdentifier(APredicate: TOPPHelpPredicate): TOPPHelpHintData;
  end;

  TOPPHelpPreviewFormCompletion = reference to procedure();

  TOPPHelpShortcutViewerExecutionResult = (erSuccess = 10000, erFailed = 10005);
  IOPPHelpShortcutViewer = interface
    ['{097D4F69-916A-4CB4-AB5F-E88D9BA1BB76}']
    function RunPredicate(const APredicate: TOPPHelpPredicate; completion: TOPPHelpPreviewFormCompletion): TOPPHelpShortcutViewerExecutionResult;
    procedure PresentModal;
  end;

  IOPPHelpViewEventListener = interface
    procedure ProgressiveEventsCountChanged(AValue: Integer; AEventName: String);
  end;

  IOPPHelpViewFullScreen = interface
    procedure loadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);
  end;

  IOPPHelpViewHint = interface

  end;

  IOPPCodable<T> = interface
    procedure EncodeToJSON(const AFileName: String; const subject: T);
    procedure DecodeFromJSON(const AFileName: String; isUTF8: Boolean; out subject: T);
  end;

implementation

end.
