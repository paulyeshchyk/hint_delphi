unit sampleForm1;

interface

uses
  cxButtonEdit, cxButtons, cxClasses, cxContainer, cxControls, cxCustomData,
  cxData, cxDataControllerConditionalFormattingRulesManagerDialog,
  cxDataStorage, cxDBData, cxDropDownEdit, cxEdit, cxFilter, cxGraphics, cxGrid,
  cxGridCustomTableView, cxGridCustomView, cxGridDBTableView, cxGridLevel,
  cxGridTableView, cxHint, cxLabel, cxListView, cxLookAndFeelPainters,
  cxLookAndFeels, cxMaskEdit, cxMemo, cxNavigator, cxSplitter, cxStyles, cxTextEdit,
  Data.DB, Datasnap.DBClient, Datasnap.Provider, dxBar,
  dxCustomHint, dxDockControl, dxDockPanel, dxScreenTip,
  System.Actions, System.Classes, System.Generics.Collections, System.SysUtils, System.Variants,
  Vcl.ActnList, Vcl.Buttons, Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.StdActns,
  Vcl.StdCtrls, Vcl.Themes,
  Winapi.CommCtrl, Winapi.Messages, Winapi.Windows,

  OPP.Help.Hint, OPP.Help.Map, OPP.Help.Meta,
  OPP.Help.System.Messaging,
  OPP.Help.System.References,
  OPP.Help.Predicate, OPP.Help.Shortcut.Server, OPP.Help.System.Error,
  SampleFormSaveState,

  dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkroom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin,
  dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue,
  dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White,
  dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp,
  dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue;

type

  TOPPHelpSaveReactionCompletion = reference to procedure(ItemToSelect: TListItem; AShouldSave: Boolean);

  TSampleForm = class(TForm)
    actionDeleteRecord: TAction;
    actionExit: TFileExit;
    ActionList1: TActionList;
    actionNewRecord: TAction;
    actionPreview: TAction;
    actionPreviewHint: TAction;
    actionPreviewShortcut: TAction;
    actionReload: TAction;
    actionSave: TAction;
    actionUndo: TAction;
    cxComboBoxHintDetailsKeywordType: TcxComboBox;
    cxComboBoxHintKeywordType: TcxComboBox;
    cxEditHintPredicateFilename: TcxButtonEdit;
    cxEditIdentifierName: TcxTextEdit;
    cxHintController: TcxHintStyleController;
    cxLabel1: TcxLabel;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    cxLabel12: TcxLabel;
    cxLabel13: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel5: TcxLabel;
    cxLabel6: TcxLabel;
    cxLabel7: TcxLabel;
    cxLabel8: TcxLabel;
    cxLabel9: TcxLabel;
    cxListView1: TcxListView;
    cxTextEditHintDetailsPredicateValue: TcxTextEdit;
    cxTextEditHintPredicateValue: TcxTextEdit;
    dxBarButton1: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarDockControl2: TdxBarDockControl;
    dxBarDockControl3: TdxBarDockControl;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarManager1Bar2: TdxBar;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N11: TMenuItem;
    N2: TMenuItem;
    N21: TMenuItem;
    N3: TMenuItem;
    N31: TMenuItem;
    N4: TMenuItem;
    OpenTextFileDialog1: TOpenTextFileDialog;
    PageControl1: TPageControl;
    PanelPreview: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    PanelIDContainer: TPanel;
    panelAddBorder: TPanel;
    panelList: TPanel;
    ShortcutDetailsKeywordTypeComboBox: TcxComboBox;
    ShortcutDetailsPredicateValueEdit: TcxTextEdit;
    ShortcutKeywordTypeComboBox: TcxComboBox;
    ShortcutPredicateFilenameEdit: TcxButtonEdit;
    ShortcutPredicateValueEdit: TcxTextEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tipsRepo: TdxScreenTipRepository;
    actionShowSettings: TAction;
    N5: TMenuItem;
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionPreviewHintExecute(Sender: TObject);
    procedure actionPreviewShortcutExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionUndoExecute(Sender: TObject);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure OnEditValueChanged(Sender: TObject);
    procedure OnIdentificatorChanged(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  protected
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure WMHOOK(var Msg: TMessage); message WM_OPPHook;
  private
    fCanChangeModificationFlag: Boolean;
    fIsIdentifierValid: Boolean;
    fIsModified: Boolean;
    fSelectedHintMap: TOPPHelpMap;
    fSelectedItem: String;
    fSelectedShortcutMap: TOPPHelpMap;
    procedure changeItemIndex(AItemIndex: Integer);
    procedure CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent);
    procedure DiscardChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure doModificationCheck(ItemToSelect: TListItem; completion: TOPPHelpSaveReactionCompletion);
    procedure doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);
    procedure FindMapsById(ItemCaption: String);
    function GetHasRecords: Boolean;
    function GetWinControlHelpKeyword(AControl: TControl): String;
    procedure OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
    procedure onHintViewsCreate(hints: TList<TOPPHelpHint>);
    procedure onMapsLoaded(AList: TList<TOPPHelpMap>; completion: TOPPHelpCompletion);

    function preferableIndexToJump(from: Integer): Integer;
    procedure RefreshPreviewButtonAction;
    procedure ReloadListView(completion: TOPPHelpCompletion);
    procedure SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure setIsIdentifierValid(AValue: Boolean);
    procedure setIsModified(AValue: Boolean);
    procedure SetSelectedHintMap(const AMap: TOPPHelpMap);
    procedure SetSelectedShortcutMap(const AMap: TOPPHelpMap);
    procedure UpdateButtonStates();
    procedure updateForm(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure updateMap(AMap: TOPPHelpMap; newIdentifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure wipeFields(identifier: TcxTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    property HasRecords: Boolean read GetHasRecords;
    property isIdentifierValid: Boolean read fIsIdentifierValid write setIsIdentifierValid;
    property isModified: Boolean read fIsModified write setIsModified;
    property SelectedHintMap: TOPPHelpMap read fSelectedHintMap write SetSelectedHintMap;
    property SelectedShortcutMap: TOPPHelpMap read fSelectedShortcutMap write SetSelectedShortcutMap;
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.dfm}
{$IFDEF DEBUG}
{$C +}
{$ENDIF}

uses
  System.UITypes,
  FormTest01,
  FormTest02,
  FormTest03,
  OPP.Help.System.Types,
  OPP.Help.Component.Enumerator,
  OPP.Help.Controls.Styler,
  OPP.Help.Hint.Reader,
  OPP.Help.Hint.Server,
  OPP.Help.Interfaces,
  OPP.Help.Log,
  OPP.Help.Shortcut.Request,
  OPP.Help.System.AppExecutor,
  OPP.Help.System.Files,
  OPP.Help.System.Hook.Keyboard,
  OPP.Help.System.Str,
  SampleFormStubsHelper,
  SampleOnly.Help.Hint.Setup,
  SampleOnly.Help.Meta.Extractor,
  SampleOnly.Help.Shortcut.Setup,
  OPP.Help.Settings.Form;

resourcestring
  SWarningListItemsIsNotSelectedNotAbleToS = 'List items is not selected. not able to set caption';
  SErrorPassedNotTControl = 'passed not TControl';
  SEmpty = '';
  SErrorCantSaveMapIdIsEmpty = 'Cant save map, id is empty';
  SErrorFindMapForHintReturnsNilMap = 'FindMap for hint returns nil map';
  SErrorFindMapForShortcutReturnsNilMap = 'FindMap for shortcut returns nil map';
  SErrorFindMapReturnsNilMap = 'FindMap returns nil map';
  SErrorHelpMapHasNoPredicate = 'HelpMap has no predicate';
  SErrorInvalidMapDetected = 'invalid map detected: %s';
  SEventDidRemovedHint = 'Did removed hint';
  SEventDidRemovedRecord = 'Did removed record';
  SEventDidRemovedShortcut = 'Did removed shortcut';
  SEventDidSavedHint = 'Did saved hint';
  SEventDidSavedShortcut = 'Did saved shortcut';
  SEventWillSaveMap = 'Will save map';
  SFileFilterPDF = 'PDF|*.pdf';
  SFileFilterRTF = 'RTF|*.rtf';
  SktBookmark = 'Переход на закладку';
  SktPage = 'Переход на страницу';
  SktSearch = 'Поиск';
  SSaveChanges = 'Сохранить изменения?';
  SWarningSelectedNilItem = 'selected nil item';
  SWarningWillNotDeleteAnythingBecauseList = 'Will not delete anything, because  listview has no selected item';

const
  kEventFlowName = 'SampleForm';
  kShortcutDropdownItemsArray: array [1 .. 2] of string = (SktSearch, SktPage);
  kHintDropdownItemsArray: array [1 .. 3] of string = (SktSearch, SktPage, SktBookmark);

procedure TSampleForm.actionDeleteRecordExecute(Sender: TObject);
var
  fState: TSampleFormSaveState;
  fItemIndexToJump: Integer;

begin

  self.isModified := false;
  fCanChangeModificationFlag := false;

  if not assigned(cxListView1.Selected) then
  begin
    eventLogger.Warning(SWarningWillNotDeleteAnythingBecauseList);
    exit;
  end;

  fItemIndexToJump := cxListView1.ItemIndex;

  fState := TSampleFormSaveState.Create;
  try
    fState.completion := procedure(AIdentifier: String)
      begin
        eventLogger.Flow(SEventDidRemovedRecord, kEventFlowName);
        ReloadListView(
          procedure
          begin
            changeItemIndex(self.preferableIndexToJump(fItemIndexToJump));
          end);
      end;
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;

    helpHintServer.RemoveHelpMap(cxListView1.Selected.Caption,
      procedure(AError: Exception)
      begin
        eventLogger.Flow(SEventDidRemovedHint, kEventFlowName);
        fState.hintWasUpdated := true;
        fState.checkAndRunMapId(SEmpty)
      end);
    helpShortcutServer.RemoveHelpMap(cxListView1.Selected.Caption,
      procedure(AError: Exception)
      begin
        eventLogger.Flow(SEventDidRemovedShortcut, kEventFlowName);
        fState.shortcutWasUpdated := true;
        fState.checkAndRunMapId(SEmpty)
      end);

  finally
    fState.Free;
  end;

end;

procedure TSampleForm.actionNewRecordExecute(Sender: TObject);
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

        changeItemIndex((cxListView1.Items.Count - 1));
        cxEditIdentifierName.SetFocus;
        cxEditIdentifierName.SelectAll;

        self.SaveChanges(self.fSelectedItem,
          procedure(ANewIdentifier: String)
          begin
            self.isModified := false;
            self.FindMapsById(ANewIdentifier);
          end);
      end;

    CreateGUID(newGUID);

    helpHintServer.CreateHelpMap(newGUID,
      procedure(const AHintMap: TOPPHelpMap)
      begin
        fState.hintWasUpdated := true;
        fState.checkAndRunMapId(GUIDToString(newGUID));
      end);

    helpShortcutServer.NewMap(newGUID,
      procedure(const AShortcutMap: TOPPHelpMap)
      begin
        fState.shortcutWasUpdated := true;
        fState.checkAndRunMapId(GUIDToString(newGUID));
      end);

  finally
    fState.Free;
  end;
end;

procedure TSampleForm.actionPreviewHintExecute(Sender: TObject);
var
  p: TPoint;
  fHint: TOPPHelpHint;
begin
  helpHintServer.FindHelpMap(fSelectedItem,
    procedure(const AMap: TOPPHelpMap)
    var
      point: TPoint;
    begin
      PanelPreview.ShowHint := true;
      PanelPreview.HelpKeyword := 'Kod_OKWED';
      PanelPreview.Hint := 'Wrong hint2';
      point := ClientToScreen(PanelPreview.ClientOrigin);
      Application.ActivateHint(point);
      // TOPPClientHintHelper.LoadHints(self,'.\Документация\hint.idx', self.cxHintController,self.tipsRepo,nil);
      // TOPPClientHintHelper.CreateHintView(fHint, PanelPreview, cxHintController, tipsRepo);
    end);

end;

procedure TSampleForm.actionPreviewShortcutExecute(Sender: TObject);
begin
  helpShortcutServer.FindHelpMap(fSelectedItem,
    procedure(const AMap: TOPPHelpMap)
    begin
      helpShortcutServer.showHelp(AMap.Predicate, vmExternal,
        procedure(APresentingResult: Exception)
        begin
          if APresentingResult = nil then
            exit;
          APresentingResult.Log();
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
        changeItemIndex(0);
    end);
end;

procedure TSampleForm.actionSaveExecute(Sender: TObject);
begin
  SaveChanges(fSelectedItem,
    procedure(ANewIdentifier: String)
    var
      listItem: TListItem;
    begin
      self.isModified := false;
      listItem := cxListView1.Selected;
      if assigned(listItem) then
        cxListView1.Selected.Caption := ANewIdentifier
      else
        eventLogger.Warning(SWarningListItemsIsNotSelectedNotAbleToS, kEventFlowName);
    end);
end;

procedure TSampleForm.actionShowSettingsExecute(Sender: TObject);
var
  formSettings: TOPPHelpSettingsForm;
begin
  formSettings := TOPPHelpSettingsForm.Create(self);
  try
    formSettings.ShowModal;
  finally
    formSettings.Free;
  end;
end;

procedure TSampleForm.actionUndoExecute(Sender: TObject);
begin
  DiscardChanges(fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      self.isModified := false;
      cxListView1.SetFocus;
    end);
end;

procedure TSampleForm.changeItemIndex(AItemIndex: Integer);
begin
  UpdateButtonStates;
  RefreshPreviewButtonAction;

  if AItemIndex < 0 then
  begin
    cxListView1.ItemIndex := AItemIndex;
    self.SelectedHintMap := nil;
    self.SelectedShortcutMap := nil;
    exit;
  end;
  cxListView1.ItemIndex := AItemIndex;
end;

procedure TSampleForm.CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent);
var
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  if not(AComponent is TControl) then
  begin
    eventLogger.Error(SErrorPassedNotTControl, kEventFlowName);
    exit;
  end;
  TControl(AComponent).ShowHint := true;

  fScreenTip := tipsRepo.Items.Add;
  fScreenTip.Width := 789;

  fScreenTip.Header.PlainText := true;
  fScreenTip.Header.Text := ''; // Заголовок

  fScreenTip.Description.PlainText := false;
  fScreenTip.Description.Text := fHint.Data.rtf;

  fScreenTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
  fScreenTipLink.ScreenTip := fScreenTip;
  fScreenTipLink.control := TControl(AComponent);
end;

procedure TSampleForm.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  OpenTextFileDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  OpenTextFileDialog1.Filter := SFileFilterRTF;
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
  OpenTextFileDialog1.Filter := SFileFilterPDF;
  if OpenTextFileDialog1.Execute(self.Handle) then
  begin
    if FileExists(OpenTextFileDialog1.filename) then
    begin
      ShortcutPredicateFilenameEdit.Text := TOPPHelpSystemFilesHelper.RelativePath(OpenTextFileDialog1.filename);
    end;
  end;

end;

procedure TSampleForm.cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  fIsIdentifierValid := false;
  fCanChangeModificationFlag := false;
  if (not assigned(Item)) or (Item = nil) then
  begin
    fSelectedItem := SEmpty;
    eventLogger.Warning(SWarningSelectedNilItem);
    exit;
  end;

  fIsIdentifierValid := true;
  if not isModified then
  begin
    fSelectedItem := Item.Caption;
    FindMapsById(Item.Caption);
    exit;
  end;

  doModificationCheck(Item, self.doSaveIfNeed);
end;

procedure TSampleForm.DiscardChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
var
  oldIdentifier: String;
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

    helpHintServer.FindHelpMap(oldIdentifier,
      procedure(const AMap: TOPPHelpMap)
      begin
        updateForm(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);

        fState.hintWasUpdated := true;
        fState.checkAndRunMap(AMap);
      end);

    helpShortcutServer.FindHelpMap(oldIdentifier,
      procedure(const AMap: TOPPHelpMap)
      begin
        if not assigned(AMap) then
        begin
          eventLogger.Error(SErrorFindMapReturnsNilMap);
          exit;
        end;
        updateForm(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);

        fState.shortcutWasUpdated := true;
        fState.checkAndRunMap(AMap);
      end);

  finally
    fState.Free;
  end;
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

  result := MessageDlg(SSaveChanges, mtwarning, [mbYes, mbNo], 0);
  completion(ItemToSelect, (result = mrYes));
end;

procedure TSampleForm.doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);
begin
  if not AShouldSave then
  begin
    self.isModified := false;
    self.FindMapsById(ItemToSelect.Caption);
    exit;
  end;

  self.SaveChanges(self.fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      self.isModified := false;
      self.FindMapsById(ANewIdentifier);
    end);
end;

procedure TSampleForm.FindMapsById(ItemCaption: String);
begin
  helpHintServer.FindHelpMap(ItemCaption,
    procedure(const AMap: TOPPHelpMap)
    begin
      self.SelectedHintMap := AMap;
    end);

  helpShortcutServer.FindHelpMap(ItemCaption,
    procedure(const AMap: TOPPHelpMap)
    begin
      self.SelectedShortcutMap := AMap;
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

  msgResult := MessageDlg(SSaveChanges, mtwarning, [mbYes, mbNo, mbCancel], 0);
  CanClose := (msgResult <> mrCancel);

  if msgResult = mrYes then
  begin
    actionSave.Execute;
  end;

end;

procedure TSampleForm.FormCreate(Sender: TObject);
var
  dropdownItem: String;
begin
  for dropdownItem in kShortcutDropdownItemsArray do
  begin
    ShortcutKeywordTypeComboBox.Properties.Items.Add(dropdownItem);
    ShortcutDetailsKeywordTypeComboBox.Properties.Items.Add(dropdownItem);
  end;

  for dropdownItem in kHintDropdownItemsArray do
  begin
    cxComboBoxHintKeywordType.Properties.Items.Add(dropdownItem);
    cxComboBoxHintDetailsKeywordType.Properties.Items.Add(dropdownItem);
  end;

  fCanChangeModificationFlag := false;
  self.isModified := false;
  self.RefreshPreviewButtonAction;

  cxListView1.Columns[0].Width := cxListView1.Width - 10;
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo,
    procedure()
    begin
      ReloadListView(
        procedure
        begin
          changeItemIndex(-1);
        end);
    end);
end;

function TSampleForm.GetHasRecords: Boolean;
begin
  result := cxListView1.Items.Count > 0;
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

procedure TSampleForm.OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TComponent;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin

  for fHint in hints do
  begin

    fControl := self.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;
    CreateScreenTip(fHint, fControl);
  end;
end;

procedure TSampleForm.OnEditValueChanged(Sender: TObject);
begin
  if fCanChangeModificationFlag then
  begin
    self.isModified := true;
  end;
end;

procedure TSampleForm.onHintViewsCreate(hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TComponent;
begin
  for fHint in hints do
  begin
    fControl := self.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;
    CreateScreenTip(fHint, fControl);
  end;
end;

procedure TSampleForm.OnIdentificatorChanged(Sender: TObject);
begin
  if fCanChangeModificationFlag then
  begin
    helpHintServer.ValidateHelpMapIdentifier(fSelectedItem, cxEditIdentifierName.Text,
      procedure(isValid: Boolean)
      begin
        self.isIdentifierValid := isValid;
        self.isModified := true;
      end);
  end;
end;

procedure TSampleForm.onMapsLoaded(AList: TList<TOPPHelpMap>; completion: TOPPHelpCompletion);
var
  Map: TOPPHelpMap;
begin
  if (not assigned(AList)) or (AList.Count = 0) then
  begin
    exit;
  end;

  for Map in AList do
  begin
    if assigned(Map) then
    begin
      if Map.isValid then
      begin
        cxListView1.AddItem(Map.ComponentIdentifier, nil);
      end else begin
        eventLogger.Error(Format(SErrorInvalidMapDetected, [Map.identifier]));
      end;
    end;
  end;

  if assigned(completion) then
    completion();

end;

procedure TSampleForm.PageControl1Change(Sender: TObject);
begin
  RefreshPreviewButtonAction;
end;

function TSampleForm.preferableIndexToJump(from: Integer): Integer;
begin
  if cxListView1.Items.Count = 0 then
  begin
    result := -1;
    exit;
  end;
  if from < cxListView1.Items.Count then
  begin
    result := from;
    exit;
  end;
  result := cxListView1.Items.Count - 1;
end;

procedure TSampleForm.RefreshPreviewButtonAction;
begin
  case PageControl1.TabIndex of
    0:
      begin
        dxBarButton7.Action := actionPreviewShortcut;
      end;
    1:
      begin
        dxBarButton7.Action := actionPreviewHint;
      end;
  end;
end;

procedure TSampleForm.ReloadListView(completion: TOPPHelpCompletion);
var
  maps: TList<TOPPHelpMap>;
begin
  cxListView1.Items.Clear;

  maps := helpHintServer.GetAvailableMaps;
  onMapsLoaded(maps, completion);

  // TOPPClientHintHelper.AvailableMaps(onMapsLoaded, completion);
end;

procedure TSampleForm.SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
var
  oldIdentifier: String;
  fState: TSampleFormSaveState;
begin
  oldIdentifier := ItemCaption;

  if Length(oldIdentifier) = 0 then
  begin
    eventLogger.Error(SErrorCantSaveMapIdIsEmpty, kEventFlowName);
    exit;
  end;

  eventLogger.Flow(SEventWillSaveMap, kEventFlowName);

  fState := TSampleFormSaveState.Create;
  try
    fState.completion := completion;
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;

    helpHintServer.FindHelpMap(oldIdentifier,
      procedure(const AMap: TOPPHelpMap)
      begin
        fState.hintWasUpdated := true;
        if not assigned(AMap) then
        begin
          eventLogger.Error(SErrorFindMapForHintReturnsNilMap, kEventFlowName);
          fState.checkAndRunMap(AMap);
          exit;
        end;

        updateMap(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
        helpHintServer.SaveHelpMaps('',
          procedure(AError: Exception)
          begin
            eventLogger.Flow(SEventDidSavedHint, kEventFlowName);
            fState.checkAndRunMap(AMap);
          end);
      end);

    helpShortcutServer.FindHelpMap(oldIdentifier,
      procedure(const AMap: TOPPHelpMap)
      begin
        fState.shortcutWasUpdated := true;
        if not assigned(AMap) then
        begin
          eventLogger.Error(SErrorFindMapForShortcutReturnsNilMap, kEventFlowName);
          fState.checkAndRunMap(AMap);
          exit;
        end;
        updateMap(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
        helpShortcutServer.SaveMaps('',
          procedure(AError: Exception)
          begin
            eventLogger.Flow(SEventDidSavedShortcut, kEventFlowName);
            fState.checkAndRunMap(AMap);
          end);
      end);

  finally
    fState.Free;
  end;
end;

procedure TSampleForm.setIsIdentifierValid(AValue: Boolean);
begin
  fIsIdentifierValid := AValue;
  actionSave.Enabled := fIsIdentifierValid;
  if fIsIdentifierValid then
    cxEditIdentifierName.Style.Color := clWindow
  else
    cxEditIdentifierName.Style.Color := clInfoBK;
end;

procedure TSampleForm.setIsModified(AValue: Boolean);
begin
  fIsModified := AValue;
  self.UpdateButtonStates;
end;

procedure TSampleForm.SetSelectedHintMap(const AMap: TOPPHelpMap);
begin
  fSelectedHintMap := AMap;
  self.updateForm(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
  actionPreviewHint.Enabled := assigned(self.SelectedHintMap);
end;

procedure TSampleForm.SetSelectedShortcutMap(const AMap: TOPPHelpMap);
begin
  fSelectedShortcutMap := AMap;
  self.updateForm(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
  actionPreviewShortcut.Enabled := assigned(self.SelectedShortcutMap);
end;

procedure TSampleForm.UpdateButtonStates;
begin
  actionSave.Enabled := self.isModified and self.isIdentifierValid;
  cxListView1.Enabled := not self.isModified;
  actionNewRecord.Enabled := not self.isModified;
  actionDeleteRecord.Enabled := self.HasRecords and (not self.isModified);
  actionReload.Enabled := self.HasRecords and (not self.isModified);
  actionPreview.Enabled := not self.isModified;
  actionUndo.Enabled := self.isModified;
  actionPreviewHint.Enabled := assigned(self.fSelectedHintMap) and (not self.isModified);
  actionPreviewShortcut.Enabled := assigned(self.fSelectedShortcutMap) and (not self.isModified);
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
    newIdentifier.Text := AMap.ComponentIdentifier;
    fPredicate := AMap.Predicate;
    if not assigned(fPredicate) then
    begin
      eventLogger.Error(SErrorHelpMapHasNoPredicate);
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

  AMap.ComponentIdentifier := newIdentifier.Text;
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

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
begin
  TOPPClientHelpShortcutHelper.showHelp(Msg);
end;

procedure TSampleForm.WMHOOK(var Msg: TMessage);
begin
  TOPPClientHintHelper.SaveHints(Screen.ActiveForm, '.\Документация\hint.idx', '.\Документация\hints.data');
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
