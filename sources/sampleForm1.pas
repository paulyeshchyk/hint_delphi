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

  OPP.System,
  OPP.Hint,
  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  OPP.dxRichEdit, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdActns, System.Actions, Vcl.ActnList, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, Data.DB, cxDBData, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxPC, dxDockControl, dxDockPanel;

type

  TSampleForm = class(TForm)
    cxHintController: TcxHintStyleController;
    Button2: TButton;
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
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  private
    { Private declarations }

    hintServer: OPPRichEditHintServer;
    hints: TList<TOPPHint>;

    procedure addTip(Hint: TOPPHint);
    procedure fillGrid();
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

uses OPP.VCL.Form.Help;

procedure TSampleForm.FormCreate(Sender: TObject);
const
  filepath: String = 'gulfstream_manual_rtf.rtf';
var
  loadResult: TOPPHintServerLoadResultType;
  fHints: TList<TOPPHint>;
  fRTF: String;
  fStream, fcxStream: TStringStream;
  fHint: TOPPHint;
begin

  fillGrid;

  hintServer := OPPRichEditHintServer.create;
  loadResult := hintServer.loadFromFile(filepath);
  if loadResult.error = nil then begin
    fHints := hintServer.GetHints(self);
    for fHint in fHints do begin
      self.addTip(fHint);
    end;
  end;

  self.restyle();
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

  fTip := tipsRepo.Items.Add;
  fTip.Header.PlainText := true;
  fTip.Header.Text := 'Заголовок';

  fTip.Description.PlainText := false;
  fTip.Description.Text := Hint.Data.rtf;

  fTip.Footer.PlainText := true;
  fTip.Footer.Text := 'Подвал';

  fTipLink := TdxScreenTipStyle(cxHintController.HintStyle).ScreenTipLinks.Add;
  fTipLink.ScreenTip := fTip;
  fTipLink.Control := fControl;
end;

procedure TSampleForm.Button1Click(Sender: TObject);
var helpForm: TOPPFormHelp;
begin
  helpForm := TOPPFormHelp.Create(self);
  helpForm.ShowModal;
end;

procedure TSampleForm.Button2Click(Sender: TObject);
begin
  ShowHintStyleEditor(cxHintController);
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
