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
  cxGridTableView, cxGridDBTableView, cxGrid,
  FireDAC.Phys.TDBXDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.TDBXBase,
  FireDAC.Phys.TDBX, dxBar, cxPC, dxDockControl, dxDockPanel, Datasnap.Provider, cxSplitter,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle,
  dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky,
  dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, cxContainer, Vcl.Menus, cxButtons,
  cxMemo, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxLabel, cxButtonEdit, Vcl.ExtDlgs;

type
  THelpMapSaveCompletion = reference to procedure(AHelpMap: TOPPHelpMap);

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
    TabSheet4: TTabSheet;
    panelAddContaner: TPanel;
    panelAddBorder: TPanel;
    GroupBox4: TGroupBox;
    cxLabel1: TcxLabel;
    cxEditHintIdentifierName: TcxTextEdit;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    Panel1: TPanel;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxComboBoxHintKeywordType: TcxComboBox;
    OpenTextFileDialog1: TOpenTextFileDialog;
    cxEditHintPredicateFilename: TcxButtonEdit;
    cxTextEditHintPredicateValue: TcxTextEdit;
    GroupBox5: TGroupBox;
    cxLabel5: TcxLabel;
    cxEditShortcutIdentifierName: TcxTextEdit;
    cxLabel6: TcxLabel;
    cxLabel7: TcxLabel;
    cxLabel8: TcxLabel;
    Panel3: TPanel;
    cxButton3: TcxButton;
    cxButton4: TcxButton;
    cxComboBoxShortcutKeywordType: TcxComboBox;
    cxEditShortcutPredicateFilename: TcxButtonEdit;
    cxTextEditShortcutPredicateValue: TcxTextEdit;
    cxLabel9: TcxLabel;
    cxTextEditShortcutDetailsPredicateValue: TcxTextEdit;
    cxComboBoxShortcutDetailsKeywordType: TcxComboBox;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    cxTextEditHintDetailsPredicateValue: TcxTextEdit;
    cxLabel12: TcxLabel;
    cxComboBoxHintDetailsKeywordType: TcxComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure externalHelpViewerButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure generateHintMappingButtonClick(Sender: TObject);
    procedure internalHelpViewerButtonClick(Sender: TObject);
    procedure dxBarButton3Click(Sender: TObject);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure cxButton4Click(Sender: TObject);
    procedure cxButton3Click(Sender: TObject);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
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

    procedure wipeFields(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit);
    procedure saveSample(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);

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
  OPP.Help.System.Files,

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

procedure TSampleForm.cxButton1Click(Sender: TObject);
begin
  saveSample(cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue,
    procedure(AHelpMap: TOPPHelpMap)
    begin
      if not assigned(AHelpMap) then
        exit;

      if helpHintServer.AddHintMap(AHelpMap) then
      begin
        if TOPPClientHintHelper.SaveMaps() = 0 then
        begin
          wipeFields(cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue);
        end else begin
          eventLogger.Error('Hintmap was not saved');
        end;
      end else begin
        eventLogger.Error('Hintmap was not added');
      end;

    end);
end;

procedure TSampleForm.cxButton2Click(Sender: TObject);
begin
  wipeFields(cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue);
end;

procedure TSampleForm.cxButton3Click(Sender: TObject);
begin
  saveSample(cxEditShortcutIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue, cxComboBoxShortcutDetailsKeywordType, cxTextEditShortcutDetailsPredicateValue,
    procedure(AHelpMap: TOPPHelpMap)
    begin
      if not assigned(AHelpMap) then
        exit;

      if helpShortcutServer.AddShortcutMap(AHelpMap) = 0 then
      begin
        if TOPPClientHelpShortcutHelper.SaveMaps() = 0 then
        begin
          wipeFields(cxEditShortcutIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue);
        end else begin
          eventLogger.Error('Hintmap was not saved');
        end;
      end else begin
        eventLogger.Error('Hintmap was not added');
      end;

    end);
end;

procedure TSampleForm.cxButton4Click(Sender: TObject);
begin
  wipeFields(cxEditShortcutIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue);
end;

procedure TSampleForm.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  OpenTextFileDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  OpenTextFileDialog1.Filter := 'RTF|*.rtf';
  if OpenTextFileDialog1.Execute(self.Handle) then
  begin
    if FileExists(OpenTextFileDialog1.filename) then
    begin
      cxEditHintPredicateFilename.Text := TOPPHelpSystemFilesHelper.RelativePath(OpenTextFileDialog1.filename);
    end;
  end;
end;

procedure TSampleForm.cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  OpenTextFileDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  OpenTextFileDialog1.Filter := 'PDF|*.pdf';
  if OpenTextFileDialog1.Execute(self.Handle) then
  begin
    if FileExists(OpenTextFileDialog1.filename) then
    begin
      cxEditShortcutPredicateFilename.Text := TOPPHelpSystemFilesHelper.RelativePath(OpenTextFileDialog1.filename);
    end;
  end;

end;

procedure TSampleForm.cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
var
  fValue: Variant;
begin

  if Sender.DataController.ItemCount = 0 then
    exit;
  // if Sender.DataController.FocusedRecordIndex < 0 then
  // exit;

  fValue := Sender.DataController.Values[Sender.DataController.FocusedRecordIndex, 0];
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

procedure TSampleForm.wipeFields(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit);
begin
  identifier.Text := '';
  filename.Text := '';
  keyword.ItemIndex := -1;
  value.Text := '';
end;

procedure TSampleForm.saveSample(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);
var
  fHelpMap: TOPPHelpMap;
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;
begin
  if not assigned(completion) then
    exit;

  fHelpMap := TOPPHelpMap.Create;
  try

    fHelpMap.identifier := identifier.Text;

    fPredicate := TOPPHelpPredicate.Create;
    try
      fPredicate.value := value.Text;
      fPredicate.keywordType := TOPPKeywordType(keyword.ItemIndex);
      fPredicate.filename := filename.Text;

      if Length(detailsvalue.Text) <> 0 then begin
        fDetailsPredicate := TOPPHelpPredicate.Create;
        fDetailsPredicate.value := detailsvalue.Text;
        fDetailsPredicate.keywordType := TOPPKeywordType(detailskeyword.ItemIndex);
        fDetailsPredicate.filename := filename.Text;
        fPredicate.predicates.Add(fDetailsPredicate);
      end;


      fHelpMap.Predicate := fPredicate;

      completion(fHelpMap);

    finally
      fPredicate.Free;
    end;
  finally
    fHelpMap.Free;
  end;
end;

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
  result.loadData(AMap.Predicate.filename);
end;

initialization

helpShortcutServer.setDefaultOnGetIdentifier(ControlHelpIdentifier);
helpHintServer.setDefaultOnHintReaderCreator(CreateHintReader);

finalization

helpHintServer.setDefaultOnHintReaderCreator(nil);
helpShortcutServer.setDefaultOnGetIdentifier(nil);

end.
