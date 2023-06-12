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
  System.Actions, System.Classes, System.Generics.Collections, System.SysUtils, System.Variants, System.StrUtils,
  System.IOUtils,
  Vcl.ActnList, Vcl.Buttons, Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.StdActns,
  Vcl.StdCtrls, Vcl.Themes,
  Winapi.CommCtrl, Winapi.Messages, Winapi.Windows,

  OPP.Help.Hint, OPP.Help.Map, OPP.Help.Meta,
  OPP.Help.System.Messaging,
  OPP.Help.System.References,
  OPP.Help.Predicate, OPP.Help.Shortcut.Server, OPP.Help.System.Error,
  OPP.Help.System.Codable.TunningEditorDefaultSettings,
  SampleFormWinControlOPPInfoExtractor,
  SampleFormSaveState, JvComponentBase, JvClipboardMonitor, cxDBEdit, cxPC, dxStatusBar;

type
  TRectHelper = record helper for TRect
    function toString(): String;
  end;

  TOPPHelpSaveReactionCompletion = reference to procedure(ItemToSelect: TListItem; AShouldSave: Boolean);
  TOPPNavigationState = set of (nsUnknown, nsLoad, nsFind, nsNavigation, nsSelection);

  TSampleForm = class(TForm)
    actionDeleteRecord: TAction;
    actionExit: TFileExit;
    ActionList1: TActionList;
    actionNewRecord: TAction;
    actionPreviewHint: TAction;
    actionReload: TAction;
    actionSave: TAction;
    actionShowSettings: TAction;
    actionUndo: TAction;
    cxHintController: TcxHintStyleController;
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
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
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
    tipsRepo: TdxScreenTipRepository;
    N6: TMenuItem;
    actionShowBuffer: TAction;
    N7: TMenuItem;
    JvClipboardMonitor1: TJvClipboardMonitor;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    dxDockSite1: TdxDockSite;
    dxDockPanelList: TdxDockPanel;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxDockPanelHint: TdxDockPanel;
    dxBarDockControl1: TdxBarDockControl;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1DBTableView1Column2: TcxGridDBColumn;
    cxGrid1Level1: TcxGridLevel;
    Panel7: TPanel;
    dxDockPanelHelp: TdxDockPanel;
    Panel5: TPanel;
    dxDockPanelIdentifier: TdxDockPanel;
    PanelIDContainer: TPanel;
    cxLabel5: TcxLabel;
    cxDBTextEdit1: TcxDBTextEdit;
    PanelPreview: TPanel;
    dxDockingManager1: TdxDockingManager;
    Panel1: TPanel;
    cxLabel2: TcxLabel;
    cxEditHintPredicateFilename: TcxButtonEdit;
    Panel2: TPanel;
    cxComboBoxHintKeywordType: TcxComboBox;
    cxLabel3: TcxLabel;
    Panel3: TPanel;
    cxLabel4: TcxLabel;
    cxTextEditHintPredicateValue: TcxTextEdit;
    Panel4: TPanel;
    cxLabel1: TcxLabel;
    Panel6: TPanel;
    cxLabel12: TcxLabel;
    cxComboBoxHintDetailsKeywordType: TcxComboBox;
    Panel8: TPanel;
    cxLabel11: TcxLabel;
    cxTextEditHintDetailsPredicateValue: TcxTextEdit;
    Panel9: TPanel;
    cxLabel6: TcxLabel;
    ShortcutPredicateFilenameEdit: TcxButtonEdit;
    Panel10: TPanel;
    cxLabel7: TcxLabel;
    ShortcutKeywordTypeComboBox: TcxComboBox;
    Panel11: TPanel;
    cxLabel8: TcxLabel;
    ShortcutPredicateValueEdit: TcxTextEdit;
    Panel12: TPanel;
    cxLabel13: TcxLabel;
    Panel13: TPanel;
    cxLabel10: TcxLabel;
    ShortcutDetailsKeywordTypeComboBox: TcxComboBox;
    Panel14: TPanel;
    cxLabel9: TcxLabel;
    ShortcutDetailsPredicateValueEdit: TcxTextEdit;
    dxBarDockControl2: TdxBarDockControl;
    dxBarManager1Bar2: TdxBar;
    dxBarManager1Bar3: TdxBar;
    dxBarDockControl3: TdxBarDockControl;
    dxBarButton11: TdxBarButton;
    actionPreviewHelp: TAction;
    dxLayoutDockSite3: TdxLayoutDockSite;
    dxStatusBar1: TdxStatusBar;
    dxBarDockControl4: TdxBarDockControl;
    dxBarManager1Bar4: TdxBar;
    N8: TMenuItem;
    NHint: TMenuItem;
    NHelp: TMenuItem;
    NIdentifier: TMenuItem;
    NList: TMenuItem;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxHorizContainerDockSite1: TdxHorizContainerDockSite;
    dxBarButton12: TdxBarButton;
    actionShowFindWindow: TAction;
    PanelFind: TPanel;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    procedure actionDeleteRecordExecute(Sender: TObject);
    procedure actionNewRecordExecute(Sender: TObject);
    procedure actionPreviewHelpExecute(Sender: TObject);
    procedure actionPreviewHintExecute(Sender: TObject);
    procedure actionReloadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionShowBufferExecute(Sender: TObject);
    procedure actionShowFindWindowExecute(Sender: TObject);
    procedure actionShowSettingsExecute(Sender: TObject);
    procedure actionUndoExecute(Sender: TObject);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure JvClipboardMonitor1Change(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure OnEditValueChanged(Sender: TObject);
    procedure cxDBTextEdit1PropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure cxGrid1DBTableView1CanFocusRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure cxGrid1Resize(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure dxDockPanelHelpVisibleChanged(Sender: TdxCustomDockControl);
    procedure dxDockPanelHintVisibleChanged(Sender: TdxCustomDockControl);
    procedure dxDockPanelListClose(Sender: TdxCustomDockControl);
    procedure dxDockPanelListVisibleChanged(Sender: TdxCustomDockControl);
    procedure dxDockPanelIdentifierClose(Sender: TdxCustomDockControl);
    procedure dxDockPanelIdentifierVisibleChanged(Sender: TdxCustomDockControl);
    procedure FormActivate(Sender: TObject);
    procedure NHelpClick(Sender: TObject);
    procedure NIdentifierClick(Sender: TObject);
    procedure NListClick(Sender: TObject);
    procedure NHintClick(Sender: TObject);
    procedure cxEditHintPredicateFilenamePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
  private
    fLoadIsInProgress: Boolean;

    fDefaultSettings: TOPPHelpHintTunningEditorDefaultSettings;

    fIsIdentifierValid: Boolean;
    fIsModified: Boolean;
    fSelectedHintMap: TOPPHelpMap;
    fSelectedItem: String;
    fSelectedShortcutMap: TOPPHelpMap;
    fResponseCount: Integer;
    fNavigationState: TOPPNavigationState;
    procedure CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent);
    procedure DiscardChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure doModificationCheck(ItemToSelect: TListItem; completion: TOPPHelpSaveReactionCompletion);
    procedure doSaveIfNeed(ItemToSelect: TListItem; AShouldSave: Boolean);
    procedure FindMapsById(ItemCaption: String);
    procedure GotResponse(ofResponses: Integer);
    function GetHasRecords: Boolean;
    function GetWinControlHelpKeyword(AControl: TControl): String;
    procedure onApplyHintMapDefaults(const AMap: POPPHelpMap);
    procedure onApplyShortcutMapDefaults(const AMap: POPPHelpMap);
    procedure OnCreateHintViewsCreate(hints: TList<TOPPHelpHint>);
    procedure onHintViewsCreate(hints: TList<TOPPHelpHint>);
    procedure onMapsLoaded(AList: TList<TOPPHelpMap>; completion: TOPPHelpCompletion);
    procedure ReloadListView(completion: TOPPHelpCompletion);
    procedure SaveChanges(ItemCaption: String; completion: THelpMapSaveCompletion);
    procedure setIsIdentifierValid(AValue: Boolean);
    procedure setIsModified(AValue: Boolean);
    procedure SetSelectedHintMap(const AMap: TOPPHelpMap);
    procedure SetSelectedShortcutMap(const AMap: TOPPHelpMap);
    procedure UpdateButtonStates();
    procedure updateForm(AMap: TOPPHelpMap; newIdentifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure updateMap(AMap: TOPPHelpMap; newIdentifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    procedure wipeFields(identifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
    function GetLayoutSettingsFileName: String;
    property HasRecords: Boolean read GetHasRecords;
    property isIdentifierValid: Boolean read fIsIdentifierValid write setIsIdentifierValid;
    property isModified: Boolean read fIsModified write setIsModified;
    property SelectedHintMap: TOPPHelpMap read fSelectedHintMap write SetSelectedHintMap;
    property SelectedShortcutMap: TOPPHelpMap read fSelectedShortcutMap write SetSelectedShortcutMap;
    property NavigationState: TOPPNavigationState read fNavigationState write fNavigationState default [nsUnknown];
    procedure GridAutoSize;
    property LayoutSettingsFileName: String read GetLayoutSettingsFileName;
    procedure customFilterText(AText: String);
  protected
    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
    procedure WMHOOK(var Msg: TMessage); message WM_OPPHook;
  end;

  TOPPObjectDataSetHelper = class helper for TClientDataSet
    function ReadList(AList: TList<TOPPHelpMap>): Boolean; // TOPPHelpMap
    procedure RecreateDataSet;
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

  OPP.ContextMenu.Edit,

  OPP.Help.Settings.Form,
  OPP.Help.System.Codable.FormSizeSettings,

  OPP.Buffer.Manager,
  OPP.Buffer.Form;

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
  SDoSaveChanges = 'Сохраните изменения, нажав на кнопку Сохранить';
  SWarningSelectedNilItem = 'selected nil item';
  SWarningWillNotDeleteAnythingBecauseList = 'Will not delete anything, because  listview has no selected item';

const
  kContext = 'SampleForm';
  kShortcutDropdownItemsArray: array [1 .. 2] of string = (SktSearch, SktPage);
  kHintDropdownItemsArray: array [1 .. 3] of string = (SktSearch, SktPage, SktBookmark);

procedure TSampleForm.actionDeleteRecordExecute(Sender: TObject);
var
  fState: TSampleFormSaveState;
  fComponentIdentifier: String;
begin

  self.isModified := false;

  fState := TSampleFormSaveState.Create;
  try
    fState.completion := procedure(AIdentifier: String)
      begin
        eventLogger.Flow(SEventDidRemovedRecord, kContext);
        ReloadListView(
          procedure
          begin
            // changeItemIndex(self.preferableIndexToJump(fItemIndexToJump));
          end);
      end;
    fState.shortcutWasUpdated := false;
    fState.hintWasUpdated := false;
    fComponentIdentifier := ClientDataSet1.FieldByName('ComponentIdentifier').AsString;

    helpHintServer.RemoveHelpMap(fComponentIdentifier,
      procedure(AError: Exception)
      begin
        eventLogger.Flow(SEventDidRemovedHint, kContext);
        fState.hintWasUpdated := true;
        fState.checkAndRunMapId(SEmpty)
      end);
    helpShortcutServer.RemoveHelpMap(fComponentIdentifier,
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

        ClientDataSet1.Insert;
        ClientDataSet1.FieldByName('ComponentIdentifier').AsString := ANewIdentifier;
        ClientDataSet1.FieldByName('Identifier').AsString := ANewIdentifier;
        ClientDataSet1.Post;

        cxDBTextEdit1.SetFocus;
        cxDBTextEdit1.SelectAll;

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

procedure TSampleForm.actionPreviewHelpExecute(Sender: TObject);
begin
  helpShortcutServer.FindHelpMap(self.fSelectedItem,
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

procedure TSampleForm.actionPreviewHintExecute(Sender: TObject);
begin
  helpHintServer.FindHelpMap(self.fSelectedItem,
    procedure(const AMap: TOPPHelpMap)
    var
      point: TPoint;
    begin
      PanelPreview.ShowHint := true;
      PanelPreview.HelpKeyword := 'Kod_OKWED';
      PanelPreview.Hint := 'Wrong hint2';
      point := ClientToScreen(PanelPreview.ClientOrigin);
      Application.ActivateHint(point);
    end);
end;

procedure TSampleForm.actionReloadExecute(Sender: TObject);
begin
  self.isModified := false;

  ReloadListView(
    procedure
    begin
    end);
end;

procedure TSampleForm.actionSaveExecute(Sender: TObject);
begin
  SaveChanges(self.fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      self.isModified := false;
    end);
end;

procedure TSampleForm.actionShowBufferExecute(Sender: TObject);
begin
  if FindWindow('TOPPBufferForm', nil) = 0 then
  begin
    TOPPBufferForm.ShowForm(nil, oppBufferManager, nil, Screen.ActiveControl);
  end
  else
    eventLogger.Debug('Cant run second instance');
end;

procedure TSampleForm.actionShowFindWindowExecute(Sender: TObject);
begin
  cxGrid1DBTableView1.Controller.ShowFindPanel;
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
  DiscardChanges(self.fSelectedItem,
    procedure(ANewIdentifier: String)
    begin
      ClientDataSet1.Edit;
      ClientDataSet1.FieldByName('ComponentIdentifier').AsString := ANewIdentifier;
      ClientDataSet1.Post;
      self.isModified := false;
    end);
end;

procedure TSampleForm.CreateScreenTip(fHint: TOPPHelpHint; AComponent: TComponent);
var
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin
  if not(AComponent is TControl) then
  begin
    eventLogger.Error(SErrorPassedNotTControl, kContext);
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

procedure TSampleForm.customFilterText(AText: String);
var
  fFilter: String;
  fQuoted: String;
  fShouldFilter: Boolean;
begin

  fShouldFilter := (Length(AText) > 0);
  self.ClientDataSet1.FilterOptions := [foCaseInsensitive];
  if not fShouldFilter then
  begin
    self.ClientDataSet1.Filter := '';
    self.ClientDataSet1.filtered := false;
    exit;
  end;

  fQuoted := QuotedStr('%' + AText + '%');
  fFilter := Format('UPPER(ComponentIdentifier) LIKE %s', [AnsiUpperCase(fQuoted)]);
  ClientDataSet1.DisableControls;
  try
    self.ClientDataSet1.filtered := true;
    self.ClientDataSet1.Filter := fFilter;
  finally
    ClientDataSet1.EnableControls;
  end;
end;

procedure TSampleForm.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  case AButtonIndex of
    0:
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
    1:
      begin
        TcxButtonEdit(Sender).ValidateEdit(true);
      end;
  end;
end;

procedure TSampleForm.cxDBTextEdit1PropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  if (nsSelection in NavigationState) then
    exit;
  if (nsLoad in NavigationState) then
    exit;
  if (nsFind in NavigationState) then
    exit;

  helpHintServer.ValidateHelpMapIdentifier(self.fSelectedItem, cxDBTextEdit1.Text,
    procedure(isValid: Boolean)
    begin
      self.isIdentifierValid := isValid;
      self.isModified := true;
    end);
end;

procedure TSampleForm.cxEditHintPredicateFilenamePropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
var
  filename: String;
begin
  if TFile.Exists(DisplayValue) then
    exit;
  filename := TOPPHelpSystemFilesHelper.AbsolutePath(DisplayValue);
  if TFile.Exists(filename) then
    exit;
  ErrorText := 'Файл указан неверно.';
  Error := true;
end;

procedure TSampleForm.cxEditShortcutPredicateFilenamePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  case AButtonIndex of
    0:
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
    1:
      begin
        TcxButtonEdit(Sender).ValidateEdit(true);
      end;
  end;
end;

procedure TSampleForm.cxGrid1DBTableView1CanFocusRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
begin
  AAllow := not isModified;
  if not isModified then
    exit;
  MessageDlg(SDoSaveChanges, mtwarning, [mbOK], 0);
end;

procedure TSampleForm.cxGrid1DBTableView1FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
var
  fComponentIdentifier: String;
begin
  fComponentIdentifier := ClientDataSet1.FieldByName('ComponentIdentifier').AsString;
  self.fIsIdentifierValid := Length(fComponentIdentifier) <> 0;

  if isModified then
  begin
    //
  end else begin
    self.fSelectedItem := fComponentIdentifier;
    FindMapsById(self.fSelectedItem);
  end;
end;

procedure TSampleForm.cxGrid1Resize(Sender: TObject);
begin
  GridAutoSize;
end;

procedure TSampleForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  dxStatusBar1.Panels[0].Text := Format('Запись: %d/%d', [ClientDataSet1.RecNo, ClientDataSet1.RecordCount]);
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
        updateForm(AMap, cxDBTextEdit1, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);

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
        updateForm(AMap, cxDBTextEdit1, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);

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

procedure TSampleForm.dxDockPanelHelpVisibleChanged(Sender: TdxCustomDockControl);
begin
  NHelp.Checked := dxDockPanelHelp.Visible;
end;

procedure TSampleForm.dxDockPanelHintVisibleChanged(Sender: TdxCustomDockControl);
begin
  NHint.Checked := dxDockPanelHint.Visible;
end;

procedure TSampleForm.dxDockPanelListClose(Sender: TdxCustomDockControl);
begin
  NList.Checked := dxDockPanelList.Visible;
end;

procedure TSampleForm.dxDockPanelListVisibleChanged(Sender: TdxCustomDockControl);
begin
  NList.Checked := dxDockPanelList.Visible;
end;

procedure TSampleForm.dxDockPanelIdentifierClose(Sender: TdxCustomDockControl);
begin
  NIdentifier.Checked := dxDockPanelIdentifier.Visible;
end;

procedure TSampleForm.dxDockPanelIdentifierVisibleChanged(Sender: TdxCustomDockControl);
begin
  NIdentifier.Checked := dxDockPanelIdentifier.Visible;
end;

procedure TSampleForm.GotResponse(ofResponses: Integer);
begin
  fResponseCount := fResponseCount + 1;
  if fResponseCount >= ofResponses then
  begin
    NavigationState := NavigationState - [nsFind];
    fResponseCount := 0;
  end;
end;

procedure TSampleForm.GridAutoSize;
var
  indicatorWidth: Integer;
  scrollIndicatorWidth: Integer;
begin
  NavigationState := NavigationState - [nsLoad];
  indicatorWidth := 0;
  if cxGrid1DBTableView1.OptionsView.Indicator then
    indicatorWidth := cxGrid1DBTableView1.OptionsView.indicatorWidth;
  scrollIndicatorWidth := 0; // cxGrid1DBTableView1.Site.VScrollBar.Width;
  cxGrid1DBTableView1Column2.Width := cxGrid1.Width - indicatorWidth - scrollIndicatorWidth;
end;

procedure TSampleForm.FindMapsById(ItemCaption: String);
begin

  fResponseCount := 0;

  NavigationState := NavigationState + [nsFind];

  helpHintServer.FindHelpMap(ItemCaption,
    procedure(const AMap: TOPPHelpMap)
    begin
      self.SelectedHintMap := AMap;
      self.GotResponse(2);
    end);

  helpShortcutServer.FindHelpMap(ItemCaption,
    procedure(const AMap: TOPPHelpMap)
    begin
      self.SelectedShortcutMap := AMap;
      self.GotResponse(2);
    end);
end;

procedure TSampleForm.FormActivate(Sender: TObject);
begin
  NHint.Checked := dxDockPanelHint.Visible;
  NList.Checked := dxDockPanelList.Visible;
  NIdentifier.Checked := dxDockPanelIdentifier.Visible;
  NHelp.Checked := dxDockPanelHelp.Visible;
end;

procedure TSampleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  self.SaveFormState;
  dxDockingManager1.SaveLayoutToIniFile(self.LayoutSettingsFileName);

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
begin

  oppBufferManager.RegisterOPPInfoExtractor(TWinControlOPPInfoExtractor.Create);

  cxDBTextEdit1.PopupMenu := TOPPContextMenuEdit.Create(self, nil, oppBufferManager);

  // settings
  fDefaultSettings := TOPPHelpSettingsForm.GetEditorDefaults();

  self.ReadFormState;
  dxDockingManager1.LoadLayoutFromIniFile(self.LayoutSettingsFileName);

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

  self.isModified := false;
  NavigationState := NavigationState - [nsSelection];

  NavigationState := NavigationState + [nsLoad];
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintController, self.tipsRepo,
    procedure()
    begin
      ReloadListView(
        procedure
        begin
          GridAutoSize;
        end);
    end);
end;

function TSampleForm.GetHasRecords: Boolean;
begin
  result := (ClientDataSet1.Active) and (ClientDataSet1.RecordCount > 0);
end;

function TSampleForm.GetLayoutSettingsFileName: String;
var
  fFileName: String;
begin
  fFileName := Format('%s.%s', [self.ClassName, 'Layout.settings']);
  result := TOPPHelpSystemFilesHelper.GetOPPSettingsPath(fFileName);
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

procedure TSampleForm.JvClipboardMonitor1Change(Sender: TObject);
begin
  oppBufferManager.ReadDataFromControl(Screen.ActiveControl);
end;

procedure TSampleForm.NHelpClick(Sender: TObject);
begin
  if dxDockPanelHelp.Visible then
    dxDockPanelHelp.Close
  else
    dxDockPanelHelp.Show;
  NHelp.Checked := dxDockPanelHelp.Visible;
end;

{ ------------ }

procedure TSampleForm.N11Click(Sender: TObject);
begin
  TformTest1.Create(self).ShowModal;
end;

procedure TSampleForm.NIdentifierClick(Sender: TObject);
begin
  if dxDockPanelIdentifier.Visible then
    dxDockPanelIdentifier.Close
  else
    dxDockPanelIdentifier.Show;
  NIdentifier.Checked := dxDockPanelIdentifier.Visible;
end;

procedure TSampleForm.NListClick(Sender: TObject);
begin
  if dxDockPanelList.Visible then
    dxDockPanelList.Close
  else
    dxDockPanelList.Show;
  NList.Checked := dxDockPanelList.Visible;
end;

procedure TSampleForm.N21Click(Sender: TObject);
begin
  TFormTest2.Create(self).ShowModal;
end;

procedure TSampleForm.N31Click(Sender: TObject);
begin
  TFormTest3.Create(self).ShowModal;
end;

procedure TSampleForm.NHintClick(Sender: TObject);
begin
  if dxDockPanelHint.Visible then
    dxDockPanelHint.Close
  else
    dxDockPanelHint.Show;
  NHint.Checked := dxDockPanelHint.Visible;
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
  AMap^.Predicate.keywordType := ktPage;
  //
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
  if (nsSelection in NavigationState) then
    exit;
  if (nsLoad in NavigationState) then
    exit;
  if (nsFind in NavigationState) then
    exit;

  self.isModified := true;
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

procedure TSampleForm.onMapsLoaded(AList: TList<TOPPHelpMap>; completion: TOPPHelpCompletion);
var
  Map: TOPPHelpMap;
begin
  cxGrid1DBTableView1.BeginUpdate();
  ClientDataSet1.ReadList(AList);
  cxGrid1DBTableView1.EndUpdate;
  UpdateButtonStates;

  if assigned(completion) then
    completion();
end;

procedure TSampleForm.ReloadListView(completion: TOPPHelpCompletion);
var
  maps: TList<TOPPHelpMap>;
begin
  maps := helpHintServer.GetAvailableMaps;
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

        updateMap(AMap, cxDBTextEdit1, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
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
        updateMap(AMap, cxDBTextEdit1, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
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
    cxDBTextEdit1.Style.Color := clWindow
  else
    cxDBTextEdit1.Style.Color := clInfoBK;
end;

procedure TSampleForm.setIsModified(AValue: Boolean);
begin
  fIsModified := AValue;
  self.UpdateButtonStates;
end;

procedure TSampleForm.SetSelectedHintMap(const AMap: TOPPHelpMap);
begin
  fSelectedHintMap := AMap;
  self.updateForm(AMap, cxDBTextEdit1, cxEditHintPredicateFilename, cxComboBoxHintKeywordType, cxTextEditHintPredicateValue, cxComboBoxHintDetailsKeywordType, cxTextEditHintDetailsPredicateValue);
  actionPreviewHint.Enabled := assigned(self.SelectedHintMap);
end;

procedure TSampleForm.SetSelectedShortcutMap(const AMap: TOPPHelpMap);
begin
  fSelectedShortcutMap := AMap;
  self.updateForm(AMap, cxDBTextEdit1, ShortcutPredicateFilenameEdit, ShortcutKeywordTypeComboBox, ShortcutPredicateValueEdit, ShortcutDetailsKeywordTypeComboBox, ShortcutDetailsPredicateValueEdit);
  actionPreviewHelp.Enabled := assigned(self.SelectedShortcutMap);
end;

procedure TSampleForm.UpdateButtonStates;
begin
  actionSave.Enabled := self.isModified and self.isIdentifierValid;
  actionNewRecord.Enabled := not self.isModified;
  actionDeleteRecord.Enabled := self.HasRecords and (not self.isModified);
  actionReload.Enabled := self.HasRecords and (not self.isModified);
  actionUndo.Enabled := self.isModified;
  actionPreviewHint.Enabled := assigned(self.fSelectedHintMap) and (not self.isModified);
  actionPreviewHelp.Enabled := assigned(self.fSelectedShortcutMap) and (not self.isModified);
end;

procedure TSampleForm.updateForm(AMap: TOPPHelpMap; newIdentifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
var
  fPredicate: TOPPHelpPredicate;
begin

  if (AMap = nil) or (not assigned(AMap)) then
  begin
    wipeFields(newIdentifier, filename, keyword, value, detailskeyword, detailsvalue);
    exit;
  end;

  try
    try
      newIdentifier.Text := AMap.ComponentIdentifier;
      fPredicate := AMap.Predicate;
      if not assigned(fPredicate) then
      begin
        eventLogger.Error(SErrorHelpMapHasNoPredicate);
      end else begin
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
      end;
    except
      on E: Exception do
      begin
        eventLogger.Error(E.Message);
      end;
    end;
  finally
    // fCanChangeModificationFlag := true;
  end;
end;

procedure TSampleForm.updateMap(AMap: TOPPHelpMap; newIdentifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
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

procedure TSampleForm.wipeFields(identifier: TcxDBTextEdit; filename: TcxButtonEdit; keyword: TcxComboBox; value: TcxTextEdit; detailskeyword: TcxComboBox; detailsvalue: TcxTextEdit);
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

{ TOPPObjectDataSetHelper }

function TOPPObjectDataSetHelper.ReadList(AList: TList<TOPPHelpMap>): Boolean;
var
  fMap: TOPPHelpMap;
begin

  RecreateDataSet;

  if (not assigned(AList)) or (AList.Count = 0) then
  begin
    exit;
  end;

  for fMap in AList do
  begin
    self.Insert;
    self.FieldByName('identifier').AsString := fMap.identifier;
    self.FieldByName('ComponentIdentifier').AsString := fMap.ComponentIdentifier;
    self.Post;
  end;

end;

procedure TOPPObjectDataSetHelper.RecreateDataSet;
begin
  self.Close;
  self.FieldDefs.Clear;

  self.FieldDefs.Add('identifier', ftString, 255);
  self.FieldDefs.Add('ComponentIdentifier', ftString, 255);
  self.CreateDataSet;
end;

{ TRectHelper }

function TRectHelper.toString: String;
begin
  result := Left.toString + ',' + top.toString + ',' + Right.toString + ',' + Bottom.toString;
end;

initialization

helpShortcutServer.setDefaultOnGetIdentifier(ControlHelpIdentifier);
helpHintServer.setDefaultOnHintReaderCreator(CreateHintReader);

finalization

helpHintServer.setDefaultOnHintReaderCreator(nil);
helpShortcutServer.setDefaultOnGetIdentifier(nil);

end.
