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

  Vcl.Graphics,
  Vcl.Themes,
  Winapi.CommCtrl,

  cxGraphics, cxStyles, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid,
  FireDAC.Phys.TDBXDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.TDBXBase,
  FireDAC.Phys.TDBX, dxBar, dxDockControl, dxDockPanel, Datasnap.Provider, cxSplitter,
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
  cxMemo, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxLabel, cxButtonEdit, Vcl.ExtDlgs, Vcl.Buttons;

type

  THelpMapSaveCompletion = reference to procedure(AHelpMap: TOPPHelpMap);

  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    tipsRepo: TdxScreenTipRepository;
    Panel2: TPanel;
    OpenTextFileDialog1: TOpenTextFileDialog;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    panelAddContaner: TPanel;
    panelAddBorder: TPanel;
    ListView1: TListView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel5: TPanel;
    cxLabel5: TcxLabel;
    cxEditShortcutIdentifierName: TcxTextEdit;
    cxLabel6: TcxLabel;
    cxEditShortcutPredicateFilename: TcxButtonEdit;
    cxLabel7: TcxLabel;
    cxComboBoxShortcutKeywordType: TcxComboBox;
    cxLabel8: TcxLabel;
    cxTextEditShortcutPredicateValue: TcxTextEdit;
    cxLabel10: TcxLabel;
    cxComboBoxShortcutDetailsKeywordType: TcxComboBox;
    cxLabel9: TcxLabel;
    cxTextEditShortcutDetailsPredicateValue: TcxTextEdit;
    cxButton4: TcxButton;
    cxButton3: TcxButton;
    TabSheet2: TTabSheet;
    Panel4: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Panel6: TPanel;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    Panel7: TPanel;
    cxComboBoxHintDetailsKeywordType: TcxComboBox;
    cxLabel12: TcxLabel;
    cxTextEditHintDetailsPredicateValue: TcxTextEdit;
    cxLabel11: TcxLabel;
    cxTextEditHintPredicateValue: TcxTextEdit;
    cxLabel4: TcxLabel;
    cxComboBoxHintKeywordType: TcxComboBox;
    cxLabel3: TcxLabel;
    cxEditHintPredicateFilename: TcxButtonEdit;
    cxLabel2: TcxLabel;
    cxEditHintIdentifierName: TcxTextEdit;
    cxLabel1: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure cxButton4Click(Sender: TObject);
    procedure cxButton3Click(Sender: TObject);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    fSelectedMap: TOPPHelpMap;

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
    procedure saveSample(oldIdentifier: String; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);
    procedure ReloadListView();

    procedure setSelectedMap(AMap: TOPPHelpMap);
    property SelectedMap: TOPPHelpMap read fSelectedMap write setSelectedMap;
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
  FormTest01,
  FormTest02,
  FormTest03,
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

procedure TSampleForm.setSelectedMap(AMap: TOPPHelpMap);
begin
  if AMap = nil then
  begin
    wipeFields(cxEditShortcutIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue);
    wipeFields(cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue);
    exit;
  end;

  cxEditHintIdentifierName.Text := AMap.identifier;
  cxEditHintPredicateFilename.Text := AMap.Predicate.filename;
  cxComboBoxHintKeywordType.SelectedItem := Integer(AMap.Predicate.keywordType);
  cxTextEditHintPredicateValue.Text := AMap.Predicate.value;
  if AMap.Predicate.predicates.Count > 0 then
  begin
    cxTextEditHintDetailsPredicateValue.Text := AMap.Predicate.predicates[0].value;
    cxComboBoxHintDetailsKeywordType.SelectedItem := Integer(AMap.Predicate.predicates[0].keywordType);
  end else begin
    cxTextEditHintDetailsPredicateValue.Text := '';
    cxComboBoxHintDetailsKeywordType.SelectedItem := 0;

  end;

end;

procedure TSampleForm.cxButton1Click(Sender: TObject);
var
  oldIdentifier: String;
begin
  oldIdentifier := ListView1.Selected.Caption;

  saveSample(oldIdentifier, cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue,
    procedure(AHelpMap: TOPPHelpMap)
    begin
      if not assigned(AHelpMap) then
        exit;

      if helpHintServer.AddHintMap(AHelpMap) then
      begin
        if TOPPClientHintHelper.SaveMaps() = 0 then
        begin
          wipeFields(cxEditHintIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue);

          ReloadListView();

          // stView1.Selected.Caption := AHelpMap.identifier;

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
var
  oldIdentifier: String;
begin
  oldIdentifier := ListView1.Selected.Caption;

  saveSample(oldIdentifier, cxEditShortcutIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue, cxComboBoxShortcutDetailsKeywordType, cxTextEditShortcutDetailsPredicateValue,
    procedure(AHelpMap: TOPPHelpMap)
    begin

      if not assigned(AHelpMap) then
      begin
        self.SelectedMap := AHelpMap;
        exit;
      end;

      if helpShortcutServer.AddShortcutMap(AHelpMap) = 0 then
      begin
        if TOPPClientHelpShortcutHelper.SaveMaps() = 0 then
        begin
          self.SelectedMap := AHelpMap;
        end else begin
          eventLogger.Error('Hintmap was not saved');
          self.SelectedMap := nil;
        end;
      end else begin
        eventLogger.Error('Hintmap was not added');
        self.SelectedMap := nil;
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

procedure TSampleForm.ReloadListView();
var
  fListHints, fListShortcuts: TList<TOPPHelpMetaIdentifierType>;
  fId: TOPPHelpMetaIdentifierType;
begin
  ListView1.Items.Clear;

  fListHints := TOPPClientHintHelper.AvailableIdentifiers();
  for fId in fListHints do
  begin
    ListView1.AddItem(fId, nil);
  end;
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo);
  ReloadListView();
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

procedure TSampleForm.N11Click(Sender: TObject);
begin
  TformTest1.Create(self).ShowModal;
end;

procedure TSampleForm.N21Click(Sender: TObject);
begin
  TFormTest2.Create(self).ShowModal;
end;

procedure TSampleForm.N31Click(Sender: TObject);
begin
  TFormTest3.Create(self).ShowModal;
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

procedure TSampleForm.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not assigned(Item) then begin
    eventLogger.Warning('selected nil item');
    exit;
  end;
  self.SelectedMap := helpHintServer.FindMap(Item.Caption);
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

procedure TSampleForm.saveSample(oldIdentifier: String; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);
var
  fHelpMap, fOldHelpMap: TOPPHelpMap;
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;
begin
  if not assigned(completion) then
    exit;

  if oldIdentifier <> '' then
  begin
    fOldHelpMap := helpHintServer.FindMap(oldIdentifier);
    if fOldHelpMap <> nil then
    begin
      helpHintServer.removeHint(oldIdentifier)
    end;
  end else begin
  end;

  fHelpMap := TOPPHelpMap.Create;

  try

    fHelpMap.identifier := newIdentifier.Text;

    fPredicate := TOPPHelpPredicate.Create;
    try
      fPredicate.value := value.Text;
      fPredicate.keywordType := TOPPKeywordType(keyword.ItemIndex);
      fPredicate.filename := filename.Text;

      if Length(detailsvalue.Text) <> 0 then
      begin
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

procedure TSampleForm.SpeedButton1Click(Sender: TObject);
begin
  ListView1.Items.Insert(0);
  ListView1.ItemIndex := 0;
  self.SelectedMap := nil;
end;

procedure TSampleForm.SpeedButton2Click(Sender: TObject);
begin
  if not assigned(ListView1.Selected) then begin
    eventLogger.Warning('going to delete nil item');
    exit;
  end;
  helpHintServer.removeHint(ListView1.Selected.Caption);
  ReloadListView;
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
