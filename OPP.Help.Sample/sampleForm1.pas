unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs,

  Vcl.ExtCtrls, cxStyles, Data.DB, dxScreenTip,
  cxClasses, dxCustomHint, cxHint,
  OPP.Help.Shortcut.Server,
  OPP.Help.Predicate,
  OPP.Help.Hint,
  OPP.Help.Meta,
  OPP.Help.Hint.Mapping,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDBData, Datasnap.DBClient, cxGridLevel, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid;

type
  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    tipsRepo: TdxScreenTipRepository;
    Panel1: TPanel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    Button1: TButton;
    Button2: TButton;
    paNavbar: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fMetaFactory: IOPPHelpMetaFactory;
    function GetWinControlHelpKeyword(AControl: TControl): String;
    { -- messages -- }
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    { -- events -- }
    function onGetShortcutIdentifier(AControl: TControl): String;
    procedure OnShowShortcutHelpResult(completionResult: TOPPHelpShortcutPresentingResult);
    { -- events -- }
    procedure OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
    function OnGetHintFactory(): IOPPHelpMetaFactory;
    procedure OnGenerateHint(AList: TList<TOPPHelpHintMap>);
    procedure OnShowHelpResult(completionResult: TOPPHelpShortcutPresentingResult);

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
  OPP.Help.Log,
  OPP.Help.Controls.Styler,
  OPP.Help.Hint.Server,

  OPP.Help.System.Str,
  OPP.Help.Shortcut.Request,
  OPP.Help.nonatomic,
  OPP.Help.View.FullScreen,
  OPP.Help.Component.Enumerator,
  OPP.Help.Meta.Factory;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  self.restyle();

  helpHintServer.getHints(self, '.\help\mapping\hints_matrix.json', self.OnCreateHintViewsCreate, self.OnGetHintFactory);

end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // fHintBuilder.Free;
  helpShortcutServer.killExternalViewer;
end;

procedure TSampleForm.Button1Click(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin
  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.keywordType := ktPage;
  fPredicate.value := '18';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate, vmInternal, OnShowHelpResult, onGetShortcutIdentifier);
end;

procedure TSampleForm.Button2Click(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin

  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.keywordType := ktPage;
  fPredicate.value := '12';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate, vmExternal, OnShowHelpResult, onGetShortcutIdentifier);
end;

procedure TSampleForm.Button3Click(Sender: TObject);
begin
  helpHintServer.GenerateMap(self, '.\help\hints\gulfstream_manual_rtf.rtf', OnGenerateHint, OnGetHintFactory);
end;

function TSampleForm.GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not Assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  eventLogger.Log(AControl.ClassName);
  if Length(AControl.HelpKeyword) <> 0 then
  begin
    result := AControl.HelpKeyword;
    exit;
  end;

  result := GetWinControlHelpKeyword(AControl.Parent);
end;

{ -- events -- }

function TSampleForm.onGetShortcutIdentifier(AControl: TControl): String;

begin
  result := GetWinControlHelpKeyword(AControl);
end;

procedure TSampleForm.OnShowShortcutHelpResult(completionResult: TOPPHelpShortcutPresentingResult);
begin
  if completionResult = prFail then
    ShowMessage('Nothing to show');
end;

{ -- events -- }

procedure TSampleForm.OnShowHelpResult(completionResult: TOPPHelpShortcutPresentingResult);
var
  strmessage: String;
begin
  strmessage := Format('show help completion result: %d', [Integer(completionResult)]);
  eventLogger.Log(strmessage);
end;

procedure TSampleForm.OnGenerateHint(AList: TList<TOPPHelpHintMap>);
begin
  helpHintServer.MergeMaps(AList);
  helpHintServer.SaveMaps('.\help\mapping\hints_matrix.json');
end;

function TSampleForm.OnGetHintFactory(): IOPPHelpMetaFactory;
begin
  result := fMetaFactory;
end;

procedure TSampleForm.OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TControl;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  eventLogger.Log('will create screentips');

  for fHint in hints do
  begin

    fControl := self.FindSubControl(fHint.Meta);
    if not Assigned(fControl) then
      exit;

    fControl.ShowHint := true;

    fScreenTip := tipsRepo.Items.Add;
    fScreenTip.Width := 789;

    fScreenTip.Header.PlainText := true;
    fScreenTip.Header.Text := ''; // Заголовок

    fScreenTip.Description.PlainText := false;
    fScreenTip.Description.Text := fHint.Data.rtf;

    fScreenTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
    fScreenTipLink.ScreenTip := fScreenTip;
    fScreenTipLink.Control := fControl;

  end;
end;

{ -- message handlers }

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(Screen.ActiveControl, Msg);
  helpShortcutServer.showHelp(fShortcutRequest, vmExternal, OnShowShortcutHelpResult, onGetShortcutIdentifier);
end;

initialization

finalization

end.
