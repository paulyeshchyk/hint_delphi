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
  System.JSON, System.IOUtils,
  OPP.Help.HintMapping,
  OPP.Help.HintServer,
  OPP.Help.HintFormHelper,
  OPP.System,
  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdActns, System.Actions, Vcl.ActnList, cxStyles, cxCustomData, cxFilter,
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

    hintServer: TOPPHelpHintServer;
    hints: TList<TOPPHint>;
    fOnHelp: THelpEvent;
    fOriginalOnHelp: THelpEvent;

    procedure fillGrid();
    function fOnHelpEventHandler(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;

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
  OPP.Help.Shortcut.Server,
  dxPDFViewer;

procedure TSampleForm.Button1Click(Sender: TObject);
begin
  helpShortcutServer.showManual(3);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  fOriginalOnHelp := Application.OnHelp;
  Application.OnHelp := fOnHelpEventHandler;

  fillGrid;

  loadHint(tipsRepo, cxHintController.HintStyle);

  self.restyle();
end;

procedure TSampleForm.FormDestroy(Sender: TObject);
begin
  Application.OnHelp := fOriginalOnHelp;
end;

function TSampleForm.fOnHelpEventHandler(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
begin
  result := helpShortcutServer.showHelp(command, data, callHelp);
end;

procedure TSampleForm.fillGrid;
var fControl: TControl;
begin

  cxGrid2TableView1.DataController.Append;
  cxGrid2TableView1.DataController.Values[0, 0] := '888.09.Test';
  cxGrid2TableView1.DataController.Values[0, 1] := '-';
  cxGrid2TableView1.DataController.Values[0, 2] := 'Изделие';
  cxGrid2TableView1.DataController.PostEditingData;
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
