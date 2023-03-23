unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs,

  Vcl.ExtCtrls, cxStyles, cxCustomData, Data.DB, dxScreenTip, OPP.Help.Shortcut.Server, cxClasses, dxCustomHint, cxHint;

type
  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    tipsRepo: TdxScreenTipRepository;
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    Kod_MKC: TEdit;
    Kod_OKWED: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure fillGrid();
  public
    { Public declarations }
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.dfm}
{$IFDEF DEBUG}
{$C +}
{$ENDIF}

uses
  OPP.Help.Vcl.Control.Styler,
  OPP.Help.Hint.FormHelper,
  OPP.Help.Hint.Server,
  OPP.Help.Shortcut.Mapping,
  OPP.Help.Shortcut.Request,
  OPP.Help.nonatomic,

  OPP.Help.Events,
  OPP.Help.System.Stream,
  OPP.Help.View.FullScreen,
  OPP.Help.System.Messaging, OPP.Help.System.Messaging.Pipe;

const
  OPPViewerProcessName: String = 'opphelppreview.exe';

procedure TSampleForm.Button1Click(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin
  fPredicate := TOPPHelpPredicate.Create;
  fPredicate.keywordType := ktPage;
  fPredicate.value := '3';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate);
end;

procedure TSampleForm.Button2Click(Sender: TObject);
var
  Pipe: TOPPMessagePipe;
  hwnd: THandle;
  Msg: TOPPHelpViewFullScreenSharedMessage;
begin

  Msg.page := 199;

  hwnd := TOPPMessagingHelper.GetProcessHandle(OPPViewerProcessName);
  if hwnd = 0 then
  begin
    hwnd := TOPPMessagingHelper.RunProcess(OPPViewerProcessName, handle);
  end;

  if hwnd = 0 then
  begin
    ShowMessage('Невозможно запустить окно помощи.');
    exit;
  end;

  Pipe := TOPPMessagePipe.Create;

  Pipe.SendRecord(hwnd, '',
    procedure(AStream: TStream)
    begin
      Msg.writeToStream(AStream);
    end);

  Pipe.Free;
end;

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(Screen.ActiveControl, Msg);
  helpShortcutServer.showHelp(fShortcutRequest);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  fillGrid;

  self.loadHint(self, tipsRepo, cxHintController.HintStyle);

  self.restyle();
end;

procedure TSampleForm.fillGrid;
begin
end;

procedure TSampleForm.cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #10) then
  begin
    Edit1.SelectAll;
  end;
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TOPPMessagingHelper.KillProcess(OPPViewerProcessName);
end;

initialization

finalization

end.
