unit OPP.Help.Interfaces;

interface

uses
  System.Classes,
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

  IOPPHelpShortcutViewer = interface
    ['{097D4F69-916A-4CB4-AB5F-E88D9BA1BB76}']
    procedure runPredicate(const APredicate: TOPPHelpPredicate);
    procedure PresentModal;
  end;

  IOPPHelpViewEventListener = interface
    procedure LoadContentStarted();
    procedure LoadContentFinished();
    procedure SearchStarted();
    procedure SearchProgress();
    procedure SearchEnded();
  end;

  IOPPHelpViewFullScreen = interface
    procedure loadContent(AStream: TMemoryStream);
    procedure setPredicate(const APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);
  end;

  IOPPHelpViewHint = interface

  end;

implementation

end.
