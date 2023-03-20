unit sampleForm1;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, dxCore, dxCoreClasses,
  dxRichEdit.Types, dxRichEdit.PlainText, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, dxGDIPlusAPI, dxGDIPlusClasses,
  dxRichEdit.Control.SpellChecker, dxRichEdit.Dialogs.EventArgs,

  dxScreenTip, cxClasses, dxCustomHint, cxHint, cxLabel, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee,
  dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy, dxSkinGlassOceans,
  dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
  dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver,
  dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray, dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue,
  dxSkinVisualStudio2013Dark, dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue,

  Vcl.ExtCtrls, cxStyles, cxCustomData,
  cxDataStorage, cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog, cxDBData, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxPC, dxDockControl, dxDockPanel,
  dxBar,

  OPP.Vcl.Controls,
  OPP.Vcl.Component,
  OPP.Help.Shortcut.Request,
  OPP.Help.Hint,
  OPP.Help.Hint.Server,
  OPP.Help.Hint.FormHelper, cxFilter, cxData, Data.DB;

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
    dxDockPanel2: TdxDockPanel;
    dxDockSite2: TdxDockSite;
    dxLayoutDockSite3: TdxLayoutDockSite;
    Panel3: TPanel;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }

    procedure WMHELP(var Msg: TWMHelp); message WM_HELP;
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

uses
  OPP.Help.Shortcut.Server,
  dxPDFViewer;

procedure TSampleForm.Button1Click(Sender: TObject);
begin
  helpShortcutServer.showManual(3);
end;

procedure TSampleForm.WMHELP(var Msg: TWMHelp);
var
  Request: TOPPHelpShortcutRequest;
begin
  Request := TOPPHelpShortcutRequest.create(Screen.ActiveControl, Msg);
  helpShortcutServer.showHelp(Request);
end;

procedure TSampleForm.FormCreate(Sender: TObject);
begin

  fillGrid;

  loadHint(self, tipsRepo, cxHintController.HintStyle);

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

procedure TSampleForm.cxHintControllerShowHint(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.cxHintControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  HintInfo.ReshowTimeout := MaxInt;
end;

procedure TSampleForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) or (Key = #10) then
  begin
    Edit1.SelectAll;
  end;
end;

initialization

finalization

end.
