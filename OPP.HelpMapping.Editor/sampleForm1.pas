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
  OPP.Help.System.Codable.TunningEditorDefaultSettings,
  SampleFormSaveState, JvComponentBase, JvClipboardMonitor;

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
    actionShowSettings: TAction;
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
    N5: TMenuItem;
    OpenTextFileDialog1: TOpenTextFileDialog;
    PageControl1: TPageControl;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    panelAddBorder: TPanel;
    PanelIDContainer: TPanel;
    panelList: TPanel;
    PanelPreview: TPanel;
    ShortcutDetailsKeywordTypeComboBox: TcxComboBox;
    ShortcutDetailsPredicateValueEdit: TcxTextEdit;
    ShortcutKeywordTypeComboBox: TcxComboBox;
    ShortcutPredicateFilenameEdit: TcxButtonEdit;
    ShortcutPredicateValueEdit: TcxTextEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tipsRepo: TdxScreenTipRepository;
    N41: TMenuItem;
    N6: TMenuItem;
    actionShowSchemeEditor: TAction;
    actionShowSchemeEditor1: TMenuItem;
    actionOnItemSelect: TAction;
    actionShowBuffer: TAction;
    actionShowBuffer1: TMenuItem;
    JvClipboardMonitor1: TJvClipboardMonitor;
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionOnItemSelectExecute(Sender: TObject);
    procedure actionPreviewHintExecute(Sender: TObject);
    procedure actionPreviewShortcutExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionShowBufferExecute(Sender: TObject);
    procedure actionShowSchemeEditorExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionUndoExecute(Sender: TObject);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure N41Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure cxEditIdentifierNamePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure cxListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure cxListView1InfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure JvClipboardMonitor1Change(Sender: TObject);
    procedure OncxControlValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
  private
    fCanChangeModificationFlag: Boolean;

    fDefaultSettings: TOPPHelpHintTunningEditorDefaultSettings;

    fIsIdentifierValid: Boolean;
    fIsModified: Boolean;
    fSelectedHintMap: TOPPHelpMap;
    fSelectedItem: String;
    fSelectedShortcutMap: TOPPHelpMap;
    procedure changeItemIndex(AItemIndex: Integer);
    function CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent): TdxScreenTipLink;
    procedure DiscardChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure doModificationCheck(ItemToSelect: TListItem; completion: TOPPHelpSaveReactionCompletion);
    procedure doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);
    procedure FindMapsById(ItemCaption: String);
    function GetHasRecords: Boolean;
    function GetWinControlHelpKeyword(AControl: TControl): String;
    procedure onApplyHintMapDefaults(const AMap: POPPHelpMap);
    procedure onApplyShortcutMapDefaults(const AMap: POPPHelpMap);
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
  protected
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure WMHOOK(var Msg: TMessage); message WM_OPPHook;
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.dfm}
{$IFDEF DEBUG}
{$C +}
{$ENDIF}

uses
  System.Generics.Defaults,
  System.UITypes,
  System.Types,
  FormTest01,
  FormTest02,
  FormTest03,
  FormTest04,
  FormSchemeEditor,
  OPP.Buffer.Form,
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
  OPP.Help.Settings.Form,
  OPP.Help.System.Codable.FormSizeSettings,
  OPP.Keyboard.Shortcut.Manager,

  OPPClient.TdxScreenTip.Helper,

  OPP.Buffer.Clipboard,
  OPP.Buffer.Manager,

  System.Threading,

  System.TypInfo, System.Rtti;

resourcestring
  SSettingsReadErrorTemplate = 'Ошибка при чтении настроек: %s';
  SWarningDefaultSettingsAreNotDefined = 'Default settings are not defined';
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
  SFileFilterRTF = 'DATA|*.data';
  SktBookmark = 'Переход на закладку';
  SktPage = 'Переход на страницу';
  SktSearch = 'Поиск';
  SSaveChanges = 'Сохранить изменения?';
  SWarningSelectedNilItem = 'selected nil item';
  SWarningWillNotDeleteAnythingBecauseList = 'Will not delete anything, because  listview has no selected item';

const
  kContext = 'SampleForm';
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
        eventLogger.Flow(SEventDidRemovedRecord, kContext);
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
        eventLogger.Flow(SEventDidRemovedHint, kContext);
        fState.hintWasUpdated := true;
        fState.checkAndRunMapId(SEmpty)
      end);
    helpShortcutServer.RemoveHelpMap(cxListView1.Selected.Caption,
      procedure(AError: Exception)
      begin
        eventLogger.Flow(SEventDidRemovedShortcut, kContext);
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

    helpHintServer.CreateHelpMap(newGUID, onApplyHintMapDefaults,
      procedure(const AHintMap: TOPPHelpMap)
      begin
        fState.hintWasUpdated := true;
        fState.checkAndRunMapId(GUIDToString(newGUID));
      end);

    helpShortcutServer.NewMap(newGUID, onApplyShortcutMapDefaults,
      procedure(const AShortcutMap: TOPPHelpMap)
      begin
        fState.shortcutWasUpdated := true;
        fState.checkAndRunMapId(GUIDToString(newGUID));
      end);

  finally
    fState.Free;
  end;
end;

procedure TSampleForm.actionOnItemSelectExecute(Sender: TObject);
begin

  fIsIdentifierValid := false;
  fCanChangeModificationFlag := false;
  if (not assigned(cxListView1.Selected)) or (cxListView1.Selected = nil) then
  begin
    fSelectedItem := SEmpty;
    eventLogger.Warning(SWarningSelectedNilItem);
    exit;
  end;

  fIsIdentifierValid := true;
  if not isModified then
  begin
    fSelectedItem := cxListView1.Selected.Caption;
    FindMapsById(cxListView1.Selected.Caption);
    exit;
  end;

  doModificationCheck(cxListView1.Selected, self.doSaveIfNeed);
end;

procedure TSampleForm.actionPreviewHintExecute(Sender: TObject);
begin
  helpHintServer.FindHelpMap(fSelectedItem,
    procedure(const AMap: TOPPHelpMap)
    var
      point: TPoint;
      fHint: TOPPHelpHint;
      fMeta: TOPPHelpMeta;
      fScreenTipLink: TdxScreenTipLink;
    begin
      fMeta.propertyName := '';
      fMeta.identifier := AMap.ComponentIdentifier;
      fHint := helpHintServer.GetHint(fMeta);

      fScreenTipLink := CreateScreenTip(fHint, PanelPreview);

      try
        point.X := PanelPreview.Left;
        point.Y := PanelPreview.Height div 4;
        point := PanelPreview.ClientToScreen(point);

        TdxScreenTipStyle(cxHintController.HintStyle).ShowScreenTip(point.X, point.Y, fScreenTipLink.ScreenTip);
        TTask.Run(
          procedure()
          begin
            Sleep(2500);
            TdxScreenTipStyle(cxHintController.HintStyle).ShowScreenTip(point.X, point.Y, nil);
          end);
      finally
        fScreenTipLink.Control := nil;
        fScreenTipLink.ScreenTip := nil;
        fScreenTipLink.Free;
      end;
    end);
end;

procedure TSampleForm.actionPreviewShortcutExecute(Sender: TObject);
begin
  helpShortcutServer.FindHelpMap(fSelectedItem,
    procedure(const AMap: TOPPHelpMap)
    begin
      helpShortcutServer.showHelp(AMap.Predicate, vmExternal,
        procedure(Error: Exception)
        begin
          if Error = nil then
            exit;
          eventLogger.Error(Error);
        end);
    end);
end;

procedure TSampleForm.actionReloadExecute(Sender: TObject);
begin
  self.isModified := false;

  ReloadListView(
    procedure
    begin
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
        eventLogger.Warning(SWarningListItemsIsNotSelectedNotAbleToS, kContext);
    end);
end;

procedure TSampleForm.actionShowBufferExecute(Sender: TObject);
begin
  // TOPPBufferForm.ShowForm(Self, Screen.ActiveControl);
  if FindWindow('TOPPBufferForm', nil) = 0 then
    TOPPBufferForm.ShowForm(self)
  else
    eventLogger.Debug('Cant run second instance');
end;

procedure TSampleForm.actionShowSchemeEditorExecute(Sender: TObject);
var
  schemeEditor: TOPPHintAttributeSchemeEditorForm;
begin
  schemeEditor := TOPPHintAttributeSchemeEditorForm.Create(self);
  try
    schemeEditor.ShowModal;
  finally
    schemeEditor.Free;
  end;

end;

procedure TSampleForm.actionShowSettingsExecute(Sender: TObject);
var
  formSettings: TOPPHelpSettingsForm;
begin
  if assigned(fDefaultSettings) then
    FreeAndNil(fDefaultSettings);

  formSettings := TOPPHelpSettingsForm.Create(self);
  try
    formSettings.ShowModal;
    fDefaultSettings := TOPPHelpSettingsForm.GetEditorDefaults();
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

function TSampleForm.CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent): TdxScreenTipLink;
var
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  result := nil;
  if not(AComponent is TControl) then
  begin
    eventLogger.Error(SErrorPassedNotTControl, kContext);
    exit;
  end;

  TControl(AComponent).ShowHint := true;

  fScreenTip := tipsRepo.Items.Add;

  fScreenTip.Header.PlainText := true;
  fScreenTip.Header.Text := ''; // Заголовок

  fScreenTip.Description.PlainText := false;
  fScreenTip.Description.Text := fHint.Data.rtf;
  fScreenTip.setAspectRatio(3.0, fHint.Data.rtf);

  fScreenTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
  fScreenTipLink.ScreenTip := fScreenTip;
  fScreenTipLink.Control := TControl(AComponent);
  Result := fScreenTipLink;
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

procedure TSampleForm.cxEditIdentifierNamePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
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

procedure TSampleForm.cxListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  //
end;

procedure TSampleForm.cxListView1InfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
begin
  InfoTip := Item.Caption;
end;

procedure TSampleForm.cxListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  actionOnItemSelect.Execute;
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
  Result: Integer;
begin
  if not assigned(completion) then
    exit;

  if not isModified then
  begin
    completion(ItemToSelect, false);
    exit;
  end;

  Result := MessageDlg(SSaveChanges, mtwarning, [mbYes, mbNo], 0);
  completion(ItemToSelect, (Result = mrYes));
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
  JvClipboardMonitor1.OnChange := nil;

  self.SaveFormState;

  helpShortcutServer.killExternalViewer;
  if assigned(fDefaultSettings) then
    fDefaultSettings.Free;
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
  fShortcut: Word;
begin
  //fShortcut := Shortcut(Ord('V'), [ssCtrl, ssShift]);
  fShortcut := oppBufferManager.Settings.GetShortCut;
  keyboardShortcutManager.registerHook(fShortcut,
    procedure
    begin
      actionShowBuffer.Execute;
    end);

  oppBufferManager.SetFormat(ifText);

  // settings
  fDefaultSettings := TOPPHelpSettingsForm.GetEditorDefaults();

  self.ReadFormState;

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
  Result := cxListView1.Items.Count > 0;
end;

function TSampleForm.GetWinControlHelpKeyword(AControl: TControl): String;
begin
  if not assigned(AControl) then
  begin
    Result := '';
    exit;
  end;

  eventLogger.Debug(AControl.ClassName);
  if Length(AControl.HelpKeyword) <> 0 then
  begin
    Result := AControl.HelpKeyword;
    exit;
  end;

  Result := GetWinControlHelpKeyword(AControl.Parent);
end;

procedure TSampleForm.JvClipboardMonitor1Change(Sender: TObject);
begin
  oppBufferManager.OnClipboardChange(Sender);
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

procedure TSampleForm.N41Click(Sender: TObject);
begin
  TFormTest4.Create(self).ShowModal;
end;

procedure TSampleForm.onApplyHintMapDefaults(const AMap: POPPHelpMap);
begin
  if not assigned(fDefaultSettings) then
  begin
    eventLogger.Error(SWarningDefaultSettingsAreNotDefined);
    exit;
  end;
  AMap^.Predicate.filename := fDefaultSettings.HintsFilePath;
  //
end;

procedure TSampleForm.onApplyShortcutMapDefaults(const AMap: POPPHelpMap);
begin
  if not assigned(fDefaultSettings) then
  begin
    eventLogger.Error(SWarningDefaultSettingsAreNotDefined);
    exit;
  end;
  AMap^.Predicate.filename := fDefaultSettings.ShortcutFilePath;
  //
end;

procedure TSampleForm.OncxControlValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  if fCanChangeModificationFlag then
  begin
    self.isModified := true;
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
        eventLogger.Warning(Format(SErrorInvalidMapDetected, [Map.identifier]), kContext);
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
    Result := -1;
    exit;
  end;
  if from < cxListView1.Items.Count then
  begin
    Result := from;
    exit;
  end;
  Result := cxListView1.Items.Count - 1;
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

  maps.Sort(TComparer<TOPPHelpMap>.Construct(
    function(const Left, Right: TOPPHelpMap): Integer
    begin
      Result := CompareStr(Left.ComponentIdentifier, Right.ComponentIdentifier);
    end));

  onMapsLoaded(maps, completion);
end;

procedure TSampleForm.SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
var
  oldIdentifier: String;
  fState: TSampleFormSaveState;
begin
  oldIdentifier := ItemCaption;

  if Length(oldIdentifier) = 0 then
  begin
    eventLogger.Error(SErrorCantSaveMapIdIsEmpty, kContext);
    exit;
  end;

  eventLogger.Flow(SEventWillSaveMap, kContext);

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
          eventLogger.Error(SErrorFindMapForHintReturnsNilMap, kContext);
          fState.checkAndRunMap(AMap);
          exit;
        end;

        updateMap(AMap, cxEditIdentifierName, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
        helpHintServer.SaveHelpMaps('',
          procedure(AError: Exception)
          begin
            eventLogger.Flow(SEventDidSavedHint, kContext);
            fState.checkAndRunMap(AMap);
          end);
      end);

    helpShortcutServer.FindHelpMap(oldIdentifier,
      procedure(const AMap: TOPPHelpMap)
      begin
        fState.shortcutWasUpdated := true;
        if not assigned(AMap) then
        begin
          eventLogger.Error(SErrorFindMapForShortcutReturnsNilMap, kContext);
          fState.checkAndRunMap(AMap);
          exit;
        end;
        updateMap(AMap, cxEditIdentifierName, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
        helpShortcutServer.SaveMaps('',
          procedure(AError: Exception)
          begin
            eventLogger.Flow(SEventDidSavedShortcut, kContext);
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
    wipeFields(nil, filename, keyword, value, detailskeyword, detailsvalue);
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
  if assigned(identifier) then
    identifier.Text := '';
  if assigned(filename) then
    filename.Text := '';
  if assigned(keyword) then
    keyword.ItemIndex := -1;
  if assigned(value) then
    value.Text := '';
  if assigned(detailskeyword) then
    detailskeyword.ItemIndex := -1;
  if assigned(detailsvalue) then
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
    Result := '';
    exit;
  end;

  eventLogger.Debug(AControl.ClassName);

  if AControl.ClassType.InheritsFrom(TForm) then
  begin
    Result := AControl.Name;
  end
  else if AControl.ClassType.InheritsFrom(TEdit) then
  begin
    Result := AControl.HelpKeyword;
  end
  else if Length(AControl.HelpKeyword) <> 0 then
  begin
    Result := AControl.HelpKeyword;
  end else begin
    Result := GetWinControlHelpKeyword(AControl.Parent);
  end;
end;

function ControlHelpIdentifier(AControl: TControl): String;
begin
  Result := GetWinControlHelpKeyword(AControl);
end;

function CreateHintReader(AMap: TOPPHelpMap): IOPPHelpHintDataReader;
begin
  Result := TOPPHelpRichtextHintReader.Create;
  Result.loadData(AMap.Predicate.filename);
end;

initialization

helpShortcutServer.setDefaultOnGetIdentifier(ControlHelpIdentifier);
helpHintServer.setDefaultOnHintReaderCreator(CreateHintReader);

finalization

helpHintServer.setDefaultOnHintReaderCreator(nil);
helpShortcutServer.setDefaultOnGetIdentifier(nil);

end.
