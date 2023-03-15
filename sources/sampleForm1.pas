unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, dxCore, dxCoreClasses, dxRichEdit.NativeApi,
  dxRichEdit.Types, dxRichEdit.PlainText, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxGDIPlusAPI, dxGDIPlusClasses, dxRichEdit.Options,
  dxRichEdit.Control, dxRichEdit.Control.SpellChecker, dxRichEdit.Dialogs.EventArgs, dxRichEdit.Platform.Win.Control,
  dxRichEdit.Control.Core,
  dxRichEdit.Api.Paragraphs,
  dxRichEdit.Api.Hyperlinks,
  dxRichEdit.Api.NativeDocument, dxScreenTip, cxClasses, dxCustomHint, cxHint, cxLabel, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue,

  cxHintEditor,

  OPP.Help.Map,
  OPP.Vcl.Form.Help,
  OPP.System,
  OPP.Hint,
  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  OPP.dxRichEdit, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdActns, System.Actions, Vcl.ActnList, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxPC, dxDockControl, dxDockPanel,
  Vcl.Menus;

type

  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    tipsRepo: TdxScreenTipRepository;
    dxDockPanel1: TdxDockPanel;
    dxDockSite1: TdxDockSite;
    Panel2: TPanel;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    cxGrid2: TcxGrid;
    cxGrid2TableView1: TcxGridTableView;
    cxGrid2TableView1Column1: TcxGridColumn;
    IGK: TcxGridColumn;
    cxGrid2TableView1Column3: TcxGridColumn;
    cxGrid2Level1: TcxGridLevel;
    dxLayoutDockSite1: TdxLayoutDockSite;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  private
    { Private declarations }

    hintServer: OPPRichEditHintServer;
    hints: TList<TOPPHint>;
    fOnHelp: THelpEvent;
    fOriginalOnHelp: THelpEvent;
    fMap: TDictionary<String, TOPPHelpMap>;

    helpForm: TOPPFormHelp;

    pdfMemoryStream: TMemoryStream;
    procedure initPDFStream;

    procedure addTip(Hint: TOPPHint);
    procedure fillGrid();
    function fh(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;

    procedure loadMap();
    procedure loadHint();

    property OnHelp: THelpEvent read fOnHelp write fOnHelp;
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
  dxPDFViewer;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  pdfMemoryStream := TMemoryStream.Create;
  initPDFStream;

  helpForm := TOPPFormHelp.Create(self);
  helpForm.stream := pdfMemoryStream;

  fOriginalOnHelp := Application.OnHelp;
  Application.OnHelp := fh;

  fillGrid;

  loadMap;
  loadHint;

  self.restyle();
end;

procedure TSampleForm.FormDestroy(Sender: TObject);
begin
  Application.OnHelp := fOriginalOnHelp;

  pdfMemoryStream.Free;
  FreeAndNil(helpForm);
end;

function TSampleForm.fh(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
var
  s: String;
  obj: TOPPHelpMap;
begin
  s := String(Data);
  try
    fMap.TryGetValue(s, obj);
  finally
    helpForm.textForSearch := obj.SearchPattern;
    helpForm.ShowModal;
  end;

  //
  CallHelp := false;
  result := true;
end;

procedure TSampleForm.initPDFStream;
begin
  pdfMemoryStream.loadFromFile('docs\гольфстрим_руководство пользователя.pdf');
  pdfMemoryStream.Position := 0;
end;

procedure TSampleForm.loadMap;
var
  map1: TOPPHelpMap;
  map2: TOPPHelpMap;
  map3: TOPPHelpMap;
begin
  fMap := TDictionary<String, TOPPHelpMap>.Create();

  map1 := TOPPHelpMap.add('Kod_MKC', 'совокупность свойств продукции, обусловливающих её пригодность');
  map2 := TOPPHelpMap.add('Kod_OKWED', '(ОКВЭД 2) ОК 029-2014');
  map3 := TOPPHelpMap.add('KodOKPD2', 'Рисунок 1303');

  fMap.add(map1.HelpKeyword, map1);
  fMap.add(map2.HelpKeyword, map2);
  fMap.add(map3.HelpKeyword, map3);
end;

procedure TSampleForm.loadHint;
const
  filepath: String = 'docs\gulfstream_manual_rtf.rtf';
var
  loadResult: TOPPHintServerLoadResultType;
  fHints: TList<TOPPHint>;
  fRTF: String;
  fStream, fcxStream: TStringStream;
  fHint: TOPPHint;
begin
  hintServer := OPPRichEditHintServer.Create;
  loadResult := hintServer.loadFromFile(filepath);
  if loadResult.error = nil then begin
    fHints := hintServer.GetHints(self);
    for fHint in fHints do begin
      self.addTip(fHint);
    end;
  end;
end;

procedure TSampleForm.fillGrid;
begin

  cxGrid2TableView1.DataController.Append;
  cxGrid2TableView1.DataController.Values[0, 0] := '888.09.Test';
  cxGrid2TableView1.DataController.Values[0, 1] := '-';
  cxGrid2TableView1.DataController.Values[0, 2] := 'Изделие';
  cxGrid2TableView1.DataController.PostEditingData;
end;

procedure TSampleForm.addTip(Hint: TOPPHint);
var
  fTip: TdxScreenTip;
  fTipLink: TdxScreenTipLink;
  fControl: TControl;
begin
  fControl := self.OPPFindComponent(Hint.meta.propertyName, Hint.meta.hintIdentifier);
  if not assigned(fControl) then
    exit;

  fTip := tipsRepo.Items.add;
  fTip.Header.PlainText := true;
  fTip.Header.Text := 'Заголовок';

  fTip.Description.PlainText := false;
  fTip.Description.Text := Hint.Data.rtf;

  fTip.Footer.PlainText := true;
  fTip.Footer.Text := 'Подвал';

  fTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.add;
  fTipLink.ScreenTip := fTip;
  fTipLink.Control := fControl;
end;

procedure TSampleForm.Button1Click(Sender: TObject);
begin
  helpForm.openPage(3);
  helpForm.ShowModal;
end;

procedure TSampleForm.cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

initialization

finalization

end.
