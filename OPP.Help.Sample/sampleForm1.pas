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
    generateHintMappingButton: TButton;
    GroupBox1: TGroupBox;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    internalHelpViewerButton: TButton;
    externalHelpViewerButton: TButton;
    paNavbar: TPanel;
    procedure externalHelpViewerButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure generateHintMappingButtonClick(Sender: TObject);
    procedure internalHelpViewerButtonClick(Sender: TObject);
  private
    { Private declarations }
    function GetWinControlHelpKeyword(AControl: TControl): String;
    { -- messages -- }
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure WMHOOK(var msg: TMessage);message WM_User + 3;
    { -- events -- }
    function onGetShortcutIdentifier(AControl: TControl): String;
    procedure OnShowShortcutHelpResult(completionResult: TOPPHelpShortcutPresentingResult);
    { -- events -- }
    procedure OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
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
//  OPP.Help.View.FullScreen,
  OPP.Help.Component.Enumerator,
  SampleOnly.Help.Meta.Factory,
  OPP.Help.Hint.Reader,
  OPP.Help.Interfaces,
  OPP.Help.System.AppExecutor,

  SampleOnly.Help.Hint.Setup,
  SampleOnly.Help.Shortcut.Setup,

  OPP.Help.System.Hook.Keyboard;

procedure TSampleForm.FormCreate(Sender: TObject);
begin
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo);
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // fHintBuilder.Free;
  helpShortcutServer.killExternalViewer;
end;

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
begin
  TOPPClientHelpShortcutHelper.showHelp(Msg);
end;

procedure TSampleForm.WMHOOK(var msg: TMessage);
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm,'.\help\mapping\hints_matrix__.json','.\help\hints\gulfstream_manual_rtf.rtf');
end;

{ ------------ }

procedure TSampleForm.generateHintMappingButtonClick(Sender: TObject);
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm,'.\help\mapping\hints_matrix__.json','.\help\hints\gulfstream_manual_rtf.rtf');
end;

procedure TSampleForm.externalHelpViewerButtonClick(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
  // fClassInfo: Pointer;
begin

  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.keywordType := ktPage;
  fPredicate.value := '12';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate, vmExternal, OnShowHelpResult);
end;

procedure TSampleForm.internalHelpViewerButtonClick(Sender: TObject);
//var
//  fPredicate: TOPPHelpPredicate;
//  fClassInfo: Pointer;
begin
  ShowMessage('Not implemented');
//  fPredicate := TOPPHelpPredicate.Create();
//  fPredicate.keywordType := ktPage;
//  fPredicate.value := '12';
//  fPredicate.fileName := '.\help\shortcuts\readme.pdf';
//  helpShortcutServer.showHelp(fPredicate, vmInternal, OnShowHelpResult);
end;

function TSampleForm.GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not assigned(AControl) then
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
var
  strmessage: String;
begin
  strmessage := Format('generated hints: %d', [Integer(AList.Count)]);
  eventLogger.Log(strmessage);
end;

procedure TSampleForm.OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TControl;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  eventLogger.Log(Format('will create screentips [%d]', [hints.Count]));

  for fHint in hints do
  begin

    fControl := self.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
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
    fScreenTipLink.control := fControl;

  end;
end;

{ -- message handlers }

function GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  eventLogger.Log(AControl.ClassName);

  if AControl.ClassType.InheritsFrom(TForm) then
  begin
    result := AControl.Name;
  end
  else if AControl.ClassType.InheritsFrom(TEdit) then
  begin
    result := AControl.HelpKeyword;
  end
  else if Length(AControl.HelpKeyword) <> 0 then
  begin
    result := AControl.HelpKeyword;
  end else begin
    result := GetWinControlHelpKeyword(AControl.Parent);
  end;
end;

function ControlHelpIdentifier(AControl: TControl): String;
begin
  result := GetWinControlHelpKeyword(AControl);
end;

function CreateHintReader(AMap: TOPPHelpHintMap): IOPPHelpHintDataReader;
begin
  result := TOPPHelpRichtextHintReader.Create;
  result.loadData(AMap.Predicate.fileName);
end;

initialization

helpShortcutServer.setDefaultOnGetIdentifier(ControlHelpIdentifier);
helpHintServer.setDefaultOnHintReaderCreator(CreateHintReader);

finalization

helpHintServer.setDefaultOnHintReaderCreator(nil);
helpShortcutServer.setDefaultOnGetIdentifier(nil);

end.
