unit OPP.Help.View.ZoomSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit, cxLabel, cxTrackBar, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.Menus, cxButtons, System.ImageList, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  OPP.Help.PreviewSettings, cxDropDownEdit;

type

  TOPPHelpPreviewZoomForm = class(TForm)
    ActionList1: TActionList;
    actionClose: TAction;
    Panel3: TPanel;
    Button1: TButton;
    ImageList1: TImageList;
    actionZoomHeight: TAction;
    actionZoomWidth: TAction;
    actionZoomTwoColumns: TAction;
    actionCustomZoom: TAction;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    cxLabel3: TcxLabel;
    cxComboBox2: TcxComboBox;
    cxLabel2: TcxLabel;
    cxComboBox1: TcxComboBox;
    cxLabel1: TcxLabel;
    cxSpinZoomFactor: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionCustomZoomExecute(Sender: TObject);
    procedure cxComboBox1PropertiesChange(Sender: TObject);
    procedure cxSpinZoomFactorPropertiesChange(Sender: TObject);
    procedure cxComboBox2PropertiesChange(Sender: TObject);
  private
    { Private declarations }
    fIsHandlingMessage: Boolean;
    fSettings: TOPPHelpPreviewSettings;
    procedure SetSettings(const Value: TOPPHelpPreviewSettings);
  public
    { Public declarations }
    property Settings: TOPPHelpPreviewSettings read fSettings write SetSettings;
  end;

var
  OPPHelpPreviewZoomForm: TOPPHelpPreviewZoomForm;

implementation

{$R *.dfm}

uses
  OPP.Help.System.Str,
  OPP.Help.System.Messaging;

procedure TOPPHelpPreviewZoomForm.FormCreate(Sender: TObject);
begin
  fIsHandlingMessage := false;
end;

procedure TOPPHelpPreviewZoomForm.SetSettings(const Value: TOPPHelpPreviewSettings);
begin
  fSettings := Value;
  if not Assigned(fSettings) then
    exit;

  fIsHandlingMessage := true;
  case fSettings.ZoomMode of
    zmFitHeight:
      cxComboBox1.ItemIndex := 0;
    zmFitWidth:
      cxComboBox1.ItemIndex := 1;
    zmTwoColumns:
      cxComboBox1.ItemIndex := 2;
    zmCustom:
      cxComboBox1.ItemIndex := 3;
  end;

  case fSettings.ScrollingType of
    stLines: cxComboBox2.ItemIndex := 0;
    stPages: cxComboBox2.ItemIndex := 1;
  end;

  cxSpinZoomFactor.Value := Value.ZoomScale;
  fIsHandlingMessage := false;
end;

procedure TOPPHelpPreviewZoomForm.actionCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TOPPHelpPreviewZoomForm.actionCustomZoomExecute(Sender: TObject);
var
  fTag: Integer;
  fHandle: THandle;
  messageResult: NativeInt;
const
  fClassName: String = 'TOPPHelpPreviewForm';
begin
  if fIsHandlingMessage then exit;

  fIsHandlingMessage := true;
  fTag := TAction(Sender).Tag;
  fHandle := FindWindow(fClassName.toWideChar(), nil);
  messageResult := SendMessage(fHandle, WM_OPPZoomFit, fTag, cxSpinZoomFactor.Value);
  cxSpinZoomFactor.Value := messageResult;
  fIsHandlingMessage := false;
end;

procedure TOPPHelpPreviewZoomForm.cxComboBox1PropertiesChange(Sender: TObject);
begin
  case cxComboBox1.ItemIndex of
    0:
      begin
        cxSpinZoomFactor.Enabled := false;
        actionZoomHeight.Execute;
      end;
    1:
      begin
        cxSpinZoomFactor.Enabled := false;
        actionZoomWidth.Execute;
      end;
    2:
      begin
        cxSpinZoomFactor.Enabled := false;
        actionZoomTwoColumns.Execute;
      end;
    3:
      begin
        cxSpinZoomFactor.Enabled := true;
        actionCustomZoom.Execute;
      end;
  end;
end;

procedure TOPPHelpPreviewZoomForm.cxComboBox2PropertiesChange(Sender: TObject);
var
  fHandle: THandle;
  messageResult: NativeInt;
const
  fClassName: String = 'TOPPHelpPreviewForm';
begin
  if fIsHandlingMessage then exit;

  fIsHandlingMessage := true;
  fHandle := FindWindow(fClassName.toWideChar(), nil);
  SendMessage(fHandle, WM_OPPScrollingType, WPARAM(cxComboBox2.ItemIndex), 0);
end;

procedure TOPPHelpPreviewZoomForm.cxSpinZoomFactorPropertiesChange(Sender: TObject);
begin
  if fIsHandlingMessage then
    exit;
  actionCustomZoom.Execute;
end;

end.
