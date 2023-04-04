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
  cxMemo, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxLabel, cxButtonEdit, Vcl.ExtDlgs, Vcl.Buttons, cxListView;

type

  THelpMapSaveCompletion = reference to procedure(ANewIdentifier: String);

  TSampleFormSaveState = class
  private
    fShortcutWasUpdated: Boolean;
    fCompletion: THelpMapSaveCompletion;
    fHintWasUpdated: Boolean;
  public
    procedure checkAndRun(AIdentifier: String);

    property hintWasUpdated: Boolean read fHintWasUpdated write fHintWasUpdated;
    property shortcutWasUpdated: Boolean read fShortcutWasUpdated write fShortcutWasUpdated;
    property completion: THelpMapSaveCompletion read fCompletion write fCompletion;
  end;

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
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel5: TPanel;
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
    Panel8: TPanel;
    cxLabel5: TcxLabel;
    cxEditIdentifierName: TcxTextEdit;
    cxListView1: TcxListView;
    SpeedButton3: TSpeedButton;
    cxLabel1: TcxLabel;
    cxLabel13: TcxLabel;
    Panel9: TPanel;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxButton1Click(Sender: TObject);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure SpeedButton3Click(Sender: TObject);
  private
    fSelectedHintMap: TOPPHelpMap;
    fSelectedShortcutMap: TOPPHelpMap;

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

    procedure wipeFields(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure saveSample(oldIdentifier: String; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);
    procedure ReloadListView();
    procedure updateMap(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure updateForm(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);

    procedure setSelectedHintMap(const AMap: TOPPHelpMap);
    property SelectedHintMap: TOPPHelpMap read fSelectedHintMap write setSelectedHintMap;

    procedure setSelectedShortcutMap(const AMap: TOPPHelpMap);
    property SelectedShortcutMap: TOPPHelpMap read fSelectedShortcutMap write setSelectedShortcutMap;
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

procedure TSampleFormSaveState.checkAndRun(AIdentifier: String);
begin
  if not fShortcutWasUpdated then
    exit;
  if not fHintWasUpdated then
    exit;
  if not assigned(completion) then
    exit;

  completion(AIdentifier);

end;

procedure TSampleForm.setSelectedShortcutMap(const AMap: TOPPHelpMap);
begin
  self.updateForm(AMap, cxEditIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue, cxComboBoxShortcutDetailsKeywordType, cxTextEditShortcutDetailsPredicateValue);
end;

procedure TSampleForm.setSelectedHintMap(const AMap: TOPPHelpMap);
begin
  self.updateForm(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
end;

procedure TSampleForm.cxButton1Click(Sender: TObject);
var
  oldIdentifier: String;
  fHelpMap, fOldHelpMap: TOPPHelpMap;
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;

  fState: TSampleFormSaveState;
begin
  oldIdentifier := cxListView1.Selected.Caption;

  if Length(oldIdentifier) = 0 then
  begin
    exit;
  end;

  fState := TSampleFormSaveState.Create;
  try
    fState.completion := procedure(ANewIdentifier: String)
      begin
        cxListView1.Selected.Caption := ANewIdentifier;
      end;
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;

    helpHintServer.FindMap(oldIdentifier,
      procedure(AMap: TOPPHelpMap)
      begin

        updateMap(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
        helpHintServer.SaveMaps('');

        fState.hintWasUpdated := true;
        fState.checkAndRun(AMap.identifier);
      end);

    helpShortcutServer.FindMap(oldIdentifier,
      procedure(AMap: TOPPHelpMap)
      begin

        updateMap(AMap, cxEditIdentifierName, cxEditShortcutPredicateFilename, cxComboBoxShortcutKeywordType, cxTextEditShortcutPredicateValue, cxComboBoxShortcutDetailsKeywordType, cxTextEditShortcutDetailsPredicateValue);
        helpShortcutServer.SaveMaps('');

        fState.shortcutWasUpdated := true;
        fState.checkAndRun(AMap.identifier);
      end);

  finally
    fState.Free;
  end;

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

procedure TSampleForm.cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  searchPattern: String;
begin

  if (not assigned(Item)) or (Item = nil) then
  begin
    eventLogger.Warning('selected nil item');
    exit;
  end;

  searchPattern := Item.Caption;

  helpHintServer.FindMap(searchPattern,
    procedure(AMap: TOPPHelpMap)
    begin
      self.SelectedHintMap := AMap;
    end);

  helpShortcutServer.FindMap(searchPattern,
    procedure(AMap: TOPPHelpMap)
    begin
      self.SelectedShortcutMap := AMap;
    end);
end;

procedure TSampleForm.ReloadListView();
var
  fListHints, fListShortcuts: TList<TOPPHelpMetaIdentifierType>;
  fId: TOPPHelpMetaIdentifierType;
  pmap: POPPHelpMap;
begin
  cxListView1.Items.Clear;

  TOPPClientHintHelper.AvailableMaps(
    procedure(AList: TList<TOPPHelpMap>)
    var
      Map: TOPPHelpMap;
    begin
      if (not assigned(AList)) or (AList.Count = 0) then
      begin
        exit;
      end;

      for Map in AList do
      begin
        pmap := POPPHelpMap(@Map);
        cxListView1.AddItem(Map.identifier, pmap);

      end;

    end);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin
  cxListView1.Columns[0].Width := cxListView1.Width - 10;
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

procedure TSampleForm.wipeFields(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
begin
  identifier.Text := '';
  filename.Text := '';
  keyword.ItemIndex := -1;
  value.Text := '';
  detailskeyword.ItemIndex := -1;
  detailsvalue.Text := '';
end;

procedure TSampleForm.saveSample(oldIdentifier: String; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit; completion: THelpMapSaveCompletion);
var
  fHelpMap, fOldHelpMap: TOPPHelpMap;
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;

  fState: TSampleFormSaveState;
begin

  if Length(oldIdentifier) = 0 then
  begin
    completion('');
    exit;
  end;

  fState := TSampleFormSaveState.Create;
  try
    fState.completion := completion;
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;

    helpHintServer.FindMap(oldIdentifier,
      procedure(AMap: TOPPHelpMap)
      begin

        updateMap(AMap, newIdentifier, filename, keyword, value, detailskeyword, detailsvalue);
        helpHintServer.SaveMaps('');

        fState.hintWasUpdated := true;
        fState.checkAndRun(AMap.identifier);
      end);

    helpShortcutServer.FindMap(oldIdentifier,
      procedure(AMap: TOPPHelpMap)
      begin

        updateMap(AMap, newIdentifier, filename, keyword, value, detailskeyword, detailsvalue);
        helpShortcutServer.SaveMaps('');

        fState.shortcutWasUpdated := true;
        fState.checkAndRun(AMap.identifier);
      end);

  finally
    fState.Free;
  end;
end;

procedure TSampleForm.updateForm(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
var
  fPredicate: TOPPHelpPredicate;
begin

  if (AMap = nil) or (not assigned(AMap)) then
  begin
    wipeFields(newIdentifier, filename, keyword, value, detailskeyword, detailsvalue);
    exit;
  end;

  try
    newIdentifier.Text := AMap.identifier;
    fPredicate := AMap.Predicate;
    if not assigned(fPredicate) then
    begin
      eventLogger.Error('HelpMap has no predicate');
    end else begin
      filename.Text := fPredicate.filename;
      keyword.SelectedItem := Integer(fPredicate.keywordType);
      value.Text := fPredicate.value;
      if fPredicate.predicates.Count > 0 then
      begin
        detailsvalue.Text := fPredicate.predicates[0].value;
        detailskeyword.SelectedItem := Integer(fPredicate.predicates[0].keywordType);
      end else begin
        detailsvalue.Text := '';
        detailskeyword.SelectedItem := 0;
      end;
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E.Message);
    end;
  end;

end;

procedure TSampleForm.updateMap(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
var
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;
begin
  if not assigned(AMap) then
    exit;

  AMap.identifier := newIdentifier.Text;
  fPredicate := AMap.Predicate;
  if assigned(fPredicate) then
  begin
    fPredicate.filename := filename.Text;
    fPredicate.keywordType := TOPPKeywordType(keyword.ItemIndex);
    fPredicate.value := value.Text;
    fPredicate.predicates.Clear;
    if Length(detailsvalue.Text) <> 0 then
    begin
      fDetailsPredicate := TOPPHelpPredicate.Create;
      fDetailsPredicate.value := detailsvalue.Text;
      fDetailsPredicate.keywordType := TOPPKeywordType(detailskeyword.ItemIndex);
      fDetailsPredicate.filename := filename.Text;
      fPredicate.predicates.Add(fDetailsPredicate);
    end;
  end;
end;

procedure TSampleForm.SpeedButton1Click(Sender: TObject);
var
  newGUID: TGUID;
  fState: TSampleFormSaveState;
begin
  fState := TSampleFormSaveState.Create;
  try
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;
    fState.completion := procedure(ANewIdentifier: String)
      var
        Item: TListItem;
      begin
        Item := cxListView1.Items.Add;
        Item.Caption := ANewIdentifier;

        cxListView1.ItemIndex := (cxListView1.Items.Count - 1);
        cxEditIdentifierName.SetFocus;
        cxEditIdentifierName.SelectAll;

      end;

    CreateGUID(newGUID);

    helpHintServer.NewMap(newGUID,
      procedure(AHintMap: TOPPHelpMap)
      begin
        fState.hintWasUpdated := true;
        fState.checkAndRun(GUIDToString(newGUID));
      end);

    helpShortcutServer.NewMap(newGUID,
      procedure(AShortcutMap: TOPPHelpMap)
      begin
        fState.shortcutWasUpdated := true;
        fState.checkAndRun(GUIDToString(newGUID));
      end);

  finally
    fState.Free;
  end;
end;

procedure TSampleForm.SpeedButton2Click(Sender: TObject);
begin
  if not assigned(cxListView1.Selected) then
  begin
    eventLogger.Warning('going to delete nil item');
    exit;
  end;
  helpHintServer.removeHint(cxListView1.Selected.Caption);
  helpShortcutServer.removeShortcut(cxListView1.Selected.Caption);
  ReloadListView;
end;

procedure TSampleForm.SpeedButton3Click(Sender: TObject);
begin
  ReloadListView();
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
