unit OPP.Help.Settings.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  OPP.Help.Settings.Value.Editor,

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkRoom, dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle,
  dxSkinFoggy, dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky,
  dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis, dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
  dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
  dxSkinsDefaultPainters, dxSkinValentine, dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinXmas2008Blue, cxContainer, cxEdit, cxListView,
  Vcl.StdActns;

type

  TOPPHelpDefaults = class;

  TOPPHelpSettingsForm = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    Button1: TButton;
    actionSave: TAction;
    cxListView1: TcxListView;
    actionEditValue: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actionEditValueExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fDefaults: TOPPHelpDefaults;
    procedure saveRESTJSON;
    procedure OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
  public
    { Public declarations }
    property Defaults: TOPPHelpDefaults read fDefaults;
  end;

  TOPPCxListView = class helper for TcxListView
  public
    procedure loadSettings(ASettings: TOPPHelpDefaults);
  end;

  TOPPHelpDefaults = class
  private
    fHintsFilePath: String;
    fShortcutFilePath: String;
  public
    property HintsFilePath: String read fHintsFilePath write fHintsFilePath;
    property ShortcutFilePath: String read fShortcutFilePath write fShortcutFilePath;
  end;

var
  OPPHelpSettingsForm: TOPPHelpSettingsForm;

implementation

uses
  OPP.Help.predicate,OPP.Help.System.Types,
  OPP.Help.log,
  System.IOUtils, System.JSON, DBXJSONReflect, REST.JSON;

{$R *.dfm}

procedure TOPPHelpSettingsForm.FormCreate(Sender: TObject);
begin
  fDefaults := TOPPHelpDefaults.Create;
  fDefaults.HintsFilePath := '.\Документация\hint.idx';
  fDefaults.ShortcutFilePath := '.\Документация\help.idx';

  cxListView1.loadSettings(fDefaults);

end;

procedure TOPPHelpSettingsForm.OnEditorCompletion(AValue: TOPPHelpSettingsValue; Saved: Boolean);
begin
  if not Saved then
    exit;
  if AValue.propertyName = 'HintsFilePath' then
    fDefaults.HintsFilePath := AValue.propertyValue;
  if AValue.propertyName = 'ShortcutFilePath' then
    fDefaults.ShortcutFilePath := AValue.propertyValue;
  cxListView1.loadSettings(fDefaults);
end;

procedure TOPPHelpSettingsForm.saveRESTJSON;
var
  fPredicate, ffPredicate : TOPPHelpPredicate;
  serializer: TJSONMarshal;
  jsonObj: TJSONObject;
  jsonString: String;
  s: String;
begin
  //


  fPredicate := TOPPHelpPredicate.Create;
  fPredicate.filename := 'zz';
  fPredicate.predicates.Add(TOPPHelpPredicate.Create('test.x',ktBookmark, 'bm'));

  s:= TJSON.ObjectToJsonString(fPredicate);
  eventLogger.Debug(s);


//  ffPredicate := TOPPHelpPredicate.Create;
  ffPredicate := TJson.JsonToObject<TOPPHelpPredicate>(s);

  serializer := TJSONMarshal.Create;

  jsonObj := serializer.marshal(fDefaults) as TJSONObject;
  try
    jsonString := TJson.JsonEncode(jsonObj);
    try
      TFile.WriteAllText('c:\defaults.json', jsonString);
    except
      on Error: Exception do
      begin
        //
      end;
    end;
  finally
    jsonObj.Free;
    FreeAndNil(serializer);
  end;
end;

procedure TOPPHelpSettingsForm.actionEditValueExecute(Sender: TObject);
var
  Editor: TOPPHelpSettingsValueEditor;
  fValue: TOPPHelpSettingsValue;
begin
  if not assigned(cxListView1.Selected) then
    exit;

  fValue.propertyCaption := cxListView1.Selected.Caption;
  fValue.propertyValue := cxListView1.Selected.Subitems[0];
  fValue.propertyName := cxListView1.Selected.Subitems[1];

  Editor := TOPPHelpSettingsValueEditor.Create(Self);
  try
    Editor.setValue(fValue);
    Editor.onCompletion := Self.OnEditorCompletion;
    Editor.ShowModal;
  finally
    Editor.Free;
  end;
end;

procedure TOPPHelpSettingsForm.actionSaveExecute(Sender: TObject);
begin

  saveRESTJSON;

  Close;
end;

procedure TOPPHelpSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fDefaults.Free;
end;

{ TOPPCxListView }

procedure TOPPCxListView.loadSettings(ASettings: TOPPHelpDefaults);
var
  fItem: TListItem;
begin
  Self.Clear;
  if not assigned(ASettings) then
    exit;

  fItem := Self.Items.Add;
  fItem.Caption := 'Путь к файлу подсказок';
  fItem.Subitems.Add(ASettings.HintsFilePath);
  fItem.Subitems.Add('HintsFilePath');
  //
  fItem := Self.Items.Add;
  fItem.Caption := 'Путь к файлу помощи';
  fItem.Subitems.Add(ASettings.ShortcutFilePath);
  fItem.Subitems.Add('ShortcutFilePath');
end;

end.
