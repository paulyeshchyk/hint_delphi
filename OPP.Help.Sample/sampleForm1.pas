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
    procedure cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fMetaFactory: IOPPHelpMetaFactory;
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure onHintViewsCreate(hints: TList<TOPPHelpHint>);

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
  OPP.Help.Hint.Mapping,

  OPP.Help.System.Str,
  OPP.Help.Shortcut.Request,
  OPP.Help.nonatomic,
  OPP.Help.View.FullScreen,
  OPP.Help.Component.Enumerator,
  OPP.Help.Meta.Factory;

procedure TSampleForm.Button1Click(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin
  fPredicate := TOPPHelpPredicate.Create;
  fPredicate.keywordType := ktPage;
  fPredicate.value := '18';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate, vmInternal,
    procedure(ACompletionResult: TOPPHelpShortcutPresentingResult)
    begin
      //
    end);
end;

procedure TSampleForm.Button2Click(Sender: TObject);
var
  fPredicate: TOPPHelpPredicate;
begin

  fPredicate := TOPPHelpPredicate.Create();
  fPredicate.keywordType := ktPage;
  fPredicate.value := '12';
  fPredicate.fileName := '.\help\shortcuts\readme.pdf';

  helpShortcutServer.showHelp(fPredicate, vmExternal,
    procedure(completionResult: TOPPHelpShortcutPresentingResult)
    begin
      //
    end);
end;

procedure TSampleForm.Button3Click(Sender: TObject);
begin
  helpHintServer.GenerateMap(self, '.\help\hints\gulfstream_manual_rtf.rtf',
    procedure(AList: TList<TOPPHelpHintMap>)
    begin
      helpHintServer.MergeMaps(AList);
      helpHintServer.SaveMaps('.\help\mapping\hints_matrix.json');
    end);
end;

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
var
  fShortcutRequest: TOPPHelpShortcutRequest;
begin
  fShortcutRequest := TOPPHelpShortcutRequest.Create(Screen.ActiveControl, Msg);
  helpShortcutServer.showHelp(fShortcutRequest, vmExternal,
    procedure(completionResult: TOPPHelpShortcutPresentingResult)
    begin
      if completionResult = prFail then
        ShowMessage('Nothing to show');
    end);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  self.restyle();

  helpHintServer.SetOnGetMetaFactory( function():IOPPHelpMetaFactory
    begin
      result := fMetaFactory;
    end
  );
  helpHintServer.getHints(self, '.\help\mapping\hints_matrix.json', self.onHintViewsCreate);

end;

procedure TSampleForm.onHintViewsCreate(hints: TList<TOPPHelpHint>);
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
    fScreenTipLink.Control := fControl;

  end;
end;

procedure TSampleForm.cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
  eventLogger.Log('will show hint');
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // fHintBuilder.Free;
  helpShortcutServer.killExternalViewer;
end;

initialization

finalization

end.
