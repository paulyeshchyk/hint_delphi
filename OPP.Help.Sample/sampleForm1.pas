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
  OPP.Help.nonatomic,

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
  cxMemo, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxLabel, cxButtonEdit, Vcl.ExtDlgs, Vcl.Buttons, cxListView,
  System.Actions, Vcl.ActnList, Vcl.StdActns;

type

  THelpMapSaveCompletion = reference to procedure(ANewIdentifier: String);
  TOPPHelpBooleanCompletion = reference to procedure(AValue: Boolean);
  TOPPHelpSaveReactionCompletion = reference to procedure(ItemToSelect: TListItem; AShouldSave: Boolean);

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
    panelList: TPanel;
    Splitter1: TSplitter;
    cxListView1: TcxListView;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    ActionList1: TActionList;
    actionNewRecord: TAction;
    actionSave: TAction;
    actionDeleteRecord: TAction;
    actionReload: TAction;
    actionPreviewHint: TAction;
    actionPreviewShortcut: TAction;
    dxBarManager1Bar2: TdxBar;
    dxBarButton1: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    actionPreview: TAction;
    actionExit: TFileExit;
    dxBarDockControl3: TdxBarDockControl;
    panelAddBorder: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel5: TPanel;
    cxLabel6: TcxLabel;
    ShortcutPredicateFilenameEdit: TcxButtonEdit;
    cxLabel7: TcxLabel;
    ShortcutKeywordTypeComboBox: TcxComboBox;
    cxLabel8: TcxLabel;
    ShortcutPredicateValueEdit: TcxTextEdit;
    cxLabel10: TcxLabel;
    ShortcutDetailsKeywordTypeComboBox: TcxComboBox;
    cxLabel9: TcxLabel;
    ShortcutDetailsPredicateValueEdit: TcxTextEdit;
    cxLabel13: TcxLabel;
    TabSheet2: TTabSheet;
    Panel4: TPanel;
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
    cxLabel1: TcxLabel;
    Panel8: TPanel;
    cxLabel5: TcxLabel;
    cxEditIdentifierName: TcxTextEdit;
    dxBarDockControl2: TdxBarDockControl;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure onControlEditing(Sender: TObject; var CanEdit: Boolean);
    procedure previewHintButtonClick(Sender: TObject);
    procedure hintsPreviewPanelResize(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionPreviewHintExecute(Sender: TObject);
    procedure actionPreviewShortcutExecute(Sender: TObject);
    procedure actionPreviewExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fSelectedItem: String;
    fSelectedHintMap: TOPPHelpMap;
    fSelectedShortcutMap: TOPPHelpMap;
    fIsModified: Boolean;
    fCanChangeModificationFlag: Boolean;
    procedure setIsModified(AValue: Boolean);

    procedure changeSelection(ItemCaption: String);
    procedure SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);

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
    procedure ReloadListView(completion: TOPPHelpCompletion);
    procedure updateMap(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure updateForm(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);

    procedure doModificationCheck(ItemToSelect: TListItem; completion: TOPPHelpSaveReactionCompletion);

    procedure setSelectedHintMap(const AMap: TOPPHelpMap);
    property SelectedHintMap: TOPPHelpMap read fSelectedHintMap write setSelectedHintMap;

    procedure setSelectedShortcutMap(const AMap: TOPPHelpMap);
    property SelectedShortcutMap: TOPPHelpMap read fSelectedShortcutMap write setSelectedShortcutMap;
    property isModified: Boolean read fIsModified write setIsModified;
    procedure onHintViewsCreate(hints: TList<TOPPHelpHint>);
  public
    { Public declarations }
  end;

var
  SampleForm: TSampleForm;

const
  dropdownItemsArray: array [1 .. 3] of string = ('Поиск', 'Переход на страницу', 'Переход на закладку');

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

procedure TSampleForm.doModificationCheck(ItemToSelect: TListItem; completion: TOPPHelpSaveReactionCompletion);
var
  result: Integer;
begin
  if not assigned(completion) then
    exit;

  if not isModified then
  begin
    completion(ItemToSelect, false);
    exit;
  end;

  result := MessageDlg('Сохранить изменения?', mtwarning, [mbYes, mbNo], 0);
  completion(ItemToSelect, (result = mrYes));
end;

procedure TSampleForm.setIsModified(AValue: Boolean);
begin
  fIsModified := AValue;
  actionSave.Enabled := fIsModified;
  cxListView1.Enabled := not fIsModified;
  actionNewRecord.Enabled := not fIsModified;
  actionReload.Enabled := not fIsModified;
  actionPreview.Enabled := not fIsModified;
end;

procedure TSampleForm.onControlEditing(Sender: TObject; var CanEdit: Boolean);
begin
  if fCanChangeModificationFlag then
  begin
    self.isModified := true;
  end;
end;

procedure TSampleForm.setSelectedShortcutMap(const AMap: TOPPHelpMap);
begin
  self.updateForm(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
end;

procedure TSampleForm.setSelectedHintMap(const AMap: TOPPHelpMap);
begin
  self.updateForm(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
end;

procedure TSampleForm.SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
var
  oldIdentifier: String;
  fHelpMap, fOldHelpMap: TOPPHelpMap;
  fPredicate, fDetailsPredicate: TOPPHelpPredicate;

  fState: TSampleFormSaveState;
begin
  oldIdentifier := ItemCaption;

  if Length(oldIdentifier) = 0 then
  begin
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

        updateMap(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
        helpHintServer.SaveMaps('');

        fState.hintWasUpdated := true;
        fState.checkAndRun(AMap.identifier);
      end);

    helpShortcutServer.FindMap(oldIdentifier,
      procedure(AMap: TOPPHelpMap)
      begin
        if not assigned(AMap) then
        begin
          eventLogger.Error('FindMap returns nil map');
          exit;
        end;
        updateMap(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
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
      ShortcutPredicateFilenameEdit.Text := TOPPHelpSystemFilesHelper.RelativePath(OpenTextFileDialog1.filename);
    end;
  end;

end;

procedure TSampleForm.actionDeleteRecordExecute(Sender: TObject);
begin

  self.isModified := false;

  if not assigned(cxListView1.Selected) then
  begin
    eventLogger.Warning('going to delete nil item');
    exit;
  end;
  helpHintServer.removeHint(cxListView1.Selected.Caption);
  helpShortcutServer.removeShortcut(cxListView1.Selected.Caption);
  ReloadListView(
    procedure
    begin
      if cxListView1.Items.Count > 0 then
        cxListView1.ItemIndex := 0;
    end);

end;

procedure TSampleForm.actionNewRecordExecute(Sender: TObject);
var
  newGUID: TGUID;
  fState: TSampleFormSaveState;
begin

  // self.isModified := true;

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

procedure TSampleForm.actionPreviewExecute(Sender: TObject);
begin
  case PageControl1.TabIndex of
    0:
      begin
        actionPreviewShortcut.Execute;
      end;
    1:
      begin
        actionPreviewHint.Execute;
      end;
  end;
end;

procedure TSampleForm.actionPreviewHintExecute(Sender: TObject);
begin
  TOPPClientHintHelper.LoadHints(self, '', cxHintController, tipsRepo,
    procedure()
    begin
      //
    end);
end;

procedure TSampleForm.actionPreviewShortcutExecute(Sender: TObject);
var
  Predicate: TOPPHelpPredicate;
begin
  helpShortcutServer.FindMap(fSelectedItem,
    procedure(AMap: TOPPHelpMap)
    begin
      helpShortcutServer.showHelp(AMap.Predicate, vmExternal,
        procedure(APresentingResult: TOPPHelpShortcutPresentingResult)
        begin
          //
        end);
    end);
end;

procedure TSampleForm.actionReloadExecute(Sender: TObject);
begin
  self.isModified := false;

  ReloadListView(
    procedure
    begin
      if cxListView1.Items.Count > 0 then
        cxListView1.ItemIndex := 0;
    end);
end;

procedure TSampleForm.actionSaveExecute(Sender: TObject);
begin
  SaveChanges(fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      self.isModified := false;
      cxListView1.Selected.Caption := ANewIdentifier;
    end);
end;

procedure TSampleForm.changeSelection(ItemCaption: String);
begin
  helpHintServer.FindMap(ItemCaption,
    procedure(AMap: TOPPHelpMap)
    begin
      self.SelectedHintMap := AMap;
    end);

  helpShortcutServer.FindMap(ItemCaption,
    procedure(AMap: TOPPHelpMap)
    begin
      self.SelectedShortcutMap := AMap;
    end);
end;

procedure TSampleForm.doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);
begin
  if not AShouldSave then
  begin
    self.isModified := false;
    self.changeSelection(ItemToSelect.Caption);
    exit;
  end;

  self.SaveChanges(self.fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      self.isModified := false;
      self.changeSelection(ANewIdentifier);
    end);
end;

procedure TSampleForm.cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  fCanChangeModificationFlag := false;
  if (not assigned(Item)) or (Item = nil) then
  begin
    fSelectedItem := '';
    eventLogger.Warning('selected nil item');
    exit;
  end;

  if not isModified then
  begin
    fSelectedItem := Item.Caption;
    changeSelection(Item.Caption);
    exit;
  end;

  doModificationCheck(Item, self.doSaveIfNeed);
end;

procedure TSampleForm.ReloadListView(completion: TOPPHelpCompletion);
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
        // pmap := POPPHelpMap(@Map);
        cxListView1.AddItem(Map.identifier, nil);
      end;

      if assigned(completion) then
        completion();

    end);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
var
  dropdownItem: String;
begin
  for dropdownItem in dropdownItemsArray do
  begin
    ShortcutKeywordTypeComboBox.Properties.Items.Add(dropdownItem);
    ShortcutDetailsKeywordTypeComboBox.Properties.Items.Add(dropdownItem);
    cxComboBoxHintKeywordType.Properties.Items.Add(dropdownItem);
    cxComboBoxHintDetailsKeywordType.Properties.Items.Add(dropdownItem);
  end;

  fCanChangeModificationFlag := false;
  self.isModified := false;

  cxListView1.Columns[0].Width := cxListView1.Width - 10;
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo, nil);
  ReloadListView(
    procedure
    begin
      if cxListView1.Items.Count > 0 then
        cxListView1.ItemIndex := 0;
    end);
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  helpShortcutServer.killExternalViewer;
end;

procedure TSampleForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  msgResult: Integer;
begin
  if not fIsModified then
  begin
    CanClose := true;
    exit;
  end;

  msgResult := MessageDlg('Сохнанить изменения?', mtwarning, [mbYes, mbNo, mbCancel], 0);
  canClose := (msgResult <> mrCancel);

  if msgResult = mrYes then begin
    actionSave.Execute;
  end;

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

procedure TSampleForm.hintsPreviewPanelResize(Sender: TObject);
begin
  //
end;

procedure TSampleForm.previewHintButtonClick(Sender: TObject);
begin

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
      fCanChangeModificationFlag := false;
      filename.Text := fPredicate.filename;
      keyword.ItemIndex := Integer(fPredicate.keywordType);
      value.Text := fPredicate.value;
      if fPredicate.predicates.Count > 0 then
      begin
        detailsvalue.Text := fPredicate.predicates[0].value;
        detailskeyword.ItemIndex := Integer(fPredicate.predicates[0].keywordType);
      end else begin
        detailsvalue.Text := '';
        detailskeyword.SelectedItem := 0;
      end;
      fCanChangeModificationFlag := true;
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

procedure TSampleForm.onHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
  fControl: TComponent;
  s: String;
begin
  for fHint in hints do
  begin
    fControl := self.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;

    TControl(fControl).ShowHint := true;

    s := fControl.ClassName;
    OutputDebugString(s.toWideChar);

    s := fControl.Name;
    OutputDebugString(s.toWideChar);
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
