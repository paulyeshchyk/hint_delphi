unit OPP.Help.Settings.Value.Editor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.StdActns,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle,
  dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky,
  dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, cxButtons, cxControls, cxContainer,
  cxEdit, cxTextEdit;

type

  TOPPHelpSettingsValue = record
    propertyCaption: String;
    propertyName: String;
    propertyValue: String;
  end;
  TOPPHelpSettingsValueEditorCompletion = reference to procedure(AValue: TOPPHelpSettingsValue; Saved: Boolean);

  TOPPHelpSettingsValueEditor = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ActionList1: TActionList;
    actionSave: TAction;
    actionClose: TAction;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxTextEdit1: TcxTextEdit;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fOnCompletion: TOPPHelpSettingsValueEditorCompletion;
    fCanSave: Boolean;
    fValue : TOPPHelpSettingsValue;
    { Private declarations }
  public
    { Public declarations }
    procedure setValue(AValue: TOPPHelpSettingsValue);
    property onCompletion: TOPPHelpSettingsValueEditorCompletion read fOnCompletion write fOnCompletion;
  end;

var
  OPPHelpSettingsValueEditor: TOPPHelpSettingsValueEditor;

implementation

{$R *.dfm}

procedure TOPPHelpSettingsValueEditor.FormCreate(Sender: TObject);
begin
  fCanSave := false;
end;

procedure TOPPHelpSettingsValueEditor.setValue(AValue: TOPPHelpSettingsValue);
begin
  fValue := AValue;
  Label1.Caption := AValue.propertyCaption;
  cxTextEdit1.Text := AValue.propertyValue;
end;

procedure TOPPHelpSettingsValueEditor.actionCloseExecute(Sender: TObject);
begin
  fCanSave := false;
  fValue.propertyValue := cxTextEdit1.Text;
  close;
end;

procedure TOPPHelpSettingsValueEditor.actionSaveExecute(Sender: TObject);
begin
  fCanSave := true;
  fValue.propertyValue := cxTextEdit1.Text;
  close;
end;

procedure TOPPHelpSettingsValueEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(fOnCompletion) then
    fOnCompletion(fValue, fCanSave);
end;

end.
