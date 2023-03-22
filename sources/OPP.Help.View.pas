unit OPP.Help.View;

interface

uses
  System.Classes,
  OPP.Help.Nonatomic;

type
  TOPPHelpViewStateChangedEvent = procedure of object;

  IOPPHelpViewEventListener = interface
    procedure SearchStarted();
    procedure SearchProgress();
    procedure SearchEnded();
  end;

  IOPPHelpViewFullScreen = interface
    procedure loadContent(AStream: TMemoryStream);
    procedure openPage(APageIndex: Integer);
    procedure setPredicate(APredicate: TOPPHelpPredicate);
    procedure addStateChangeListener(AListener: IOPPHelpViewEventListener);
    procedure removeStateChangeListener(AListener: IOPPHelpViewEventListener);
  end;

  IOPPHelpViewHint = interface

  end;

implementation

end.
