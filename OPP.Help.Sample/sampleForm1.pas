unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs,

  Vcl.ExtCtrls, Vcl.ComCtrls, Data.DB, dxScreenTip,
  Datasnap.DBClient,
  cxClasses, dxCustomHint, cxHint,
  OPP.Help.Shortcut.Server,
  OPP.Help.Predicate,
  OPP.Help.Hint,
  OPP.Help.Meta,
  OPP.Help.Map,

  cxGraphics, cxStyles, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, dxSkinsCore, dxSkinBasic, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkroom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle,
  dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky,
  dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinOffice2019Black,
  dxSkinOffice2019Colorful, dxSkinOffice2019DarkGray, dxSkinOffice2019White, dxSkinPumpkin, dxSkinSeven,
  dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust, dxSkinSummer2008,
  dxSkinTheAsphaltWorld, dxSkinTheBezier, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue,
  dxDateRanges, dxScrollbarAnnotations, FireDAC.Phys.TDBXDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.TDBXBase,
  FireDAC.Phys.TDBX, dxBar, cxPC, dxDockControl, dxDockPanel, Datasnap.Provider, cxSplitter;

type
  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    tipsRepo: TdxScreenTipRepository;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    internalHelpViewerButton: TButton;
    externalHelpViewerButton: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    generateHintMappingButton: TButton;
    TabSheet2: TTabSheet;
    GroupBox3: TGroupBox;
    Button1: TButton;
    Memo1: TMemo;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    TabSheet3: TTabSheet;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton1: TdxBarButton;
    recordsDataset: TClientDataSet;
    DataSource1: TDataSource;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarManager1Bar2: TdxBar;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxDockPanel1: TdxDockPanel;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxDockSite2: TdxDockSite;
    dxDockPanel2: TdxDockPanel;
    dxLayoutDockSite2: TdxLayoutDockSite;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxGrid2DBTableView1: TcxGridDBTableView;
    cxGrid2Level1: TcxGridLevel;
    cxGrid2: TcxGrid;
    DataSource2: TDataSource;
    dxBarDockControl1: TdxBarDockControl;
    dxBarDockControl2: TdxBarDockControl;
    dxBarManager1Bar3: TdxBar;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    predicatesDataset: TClientDataSet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure cxGrid1DBTableView1FocusedRecordChanged(Sender:
        TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord:
        TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure externalHelpViewerButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure generateHintMappingButtonClick(Sender: TObject);
    procedure internalHelpViewerButtonClick(Sender: TObject);
    procedure dxBarButton3Click(Sender: TObject);
  private

    { Private declarations }
    function GetWinControlHelpKeyword(AControl: TControl): String;
    { -- messages -- }
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure WMHOOK(var Msg: TMessage); message WM_User + 3;
    { -- events -- }
    function onGetShortcutIdentifier(AControl: TControl): String;
    procedure OnShowShortcutHelpResult(completionResult: TOPPHelpShortcutPresentingResult);
    { -- events -- }
    procedure OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);

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
  sampleFormHelper,
  OPP.Help.Log,
  OPP.Help.Controls.Styler,
  OPP.Help.Hint.Server,

  OPP.Help.System.Str,
  OPP.Help.Shortcut.Request,
  OPP.Help.nonatomic,
  // OPP.Help.View.FullScreen,
  OPP.Help.Component.Enumerator,
  SampleOnly.Help.Meta.Factory,
  OPP.Help.Hint.Reader,
  OPP.Help.Interfaces,
  OPP.Help.System.AppExecutor,

  SampleOnly.Help.Hint.Setup,
  SampleOnly.Help.Shortcut.Setup,

  OPP.Help.Map.Filereader,

  OPP.Help.System.Hook.Keyboard;

procedure TSampleForm.Button1Click(Sender: TObject);
begin
  TSampleFormHelper.savePredicateToStream;
end;

procedure TSampleForm.Button2Click(Sender: TObject);
begin
  TSampleFormHelper.savePredicateToFile;
end;

procedure TSampleForm.Button4Click(Sender: TObject);
begin
  TSampleFormHelper.copyPredicate;
end;

procedure TSampleForm.Button5Click(Sender: TObject);
begin
  TSampleFormHelper.readPredicateFromFile;
end;

procedure TSampleForm.cxGrid1DBTableView1FocusedRecordChanged(Sender:
    TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord:
    TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
var
  fValue: Variant;
begin

  if Sender.DataController.ItemCount = 0 then
  exit;
//  if Sender.DataController.FocusedRecordIndex < 0 then
//    exit;

  fValue := Sender.DataController.Values[Sender.DataController.FocusedRecordIndex,0];
  TSampleFormHelper.loadPredicatesDataset(predicatesDataset, fValue);
end;

procedure TSampleForm.dxBarButton3Click(Sender: TObject);
begin
  TSampleFormHelper.makeRecordsDataset(recordsDataset);
  TSampleFormHelper.makePredicatesDataset(predicatesDataset);

  cxGrid1DBTableView1.DataController.CreateAllItems();
  cxGrid1DBTableView1.ApplyBestFit();

  cxGrid2DBTableView1.DataController.CreateAllItems();
  cxGrid2DBTableView1.ApplyBestFit();

end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo);
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  helpShortcutServer.killExternalViewer;
end;

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
begin
  TOPPClientHelpShortcutHelper.showHelp(Msg);
end;

procedure TSampleForm.WMHOOK(var Msg: TMessage);
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm, '.\help\mapping\hints_matrix__.json', '.\help\hints\gulfstream_manual_rtf.rtf');
end;

{ ------------ }

procedure TSampleForm.generateHintMappingButtonClick(Sender: TObject);
begin
  TSampleFormHelper.generateHintMapping;
end;

procedure TSampleForm.externalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormHelper.openExternalHelp;
end;

procedure TSampleForm.internalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormHelper.openInternalHelp;
end;

function TSampleForm.GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not assigned(AControl) then
  begin
    result := '';
    exit;
  end;

  eventLogger.Debug(AControl.ClassName);
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

procedure TSampleForm.OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TComponent;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  eventLogger.Debug(Format('will create screentips [%d]', [hints.Count]));

  for fHint in hints do
  begin

    fControl := self.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;

    TControl(fControl).ShowHint := true;

    fScreenTip := tipsRepo.Items.Add;
    fScreenTip.Width := 789;

    fScreenTip.Header.PlainText := true;
    fScreenTip.Header.Text := ''; // Заголовок

    fScreenTip.Description.PlainText := false;
    fScreenTip.Description.Text := fHint.Data.rtf;

    fScreenTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
    fScreenTipLink.ScreenTip := fScreenTip;
    fScreenTipLink.control := TControl(fControl);

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

  eventLogger.Debug(AControl.ClassName);

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

function CreateHintReader(AMap: TOPPHelpMap): IOPPHelpHintDataReader;
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
