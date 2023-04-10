unit FormTest01;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, dxScreenTip, dxCustomHint, cxHint, cxClasses;

type
  TFormTest1 = class(TForm)
    GroupBox11: TGroupBox;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    internalHelpViewerButton: TButton;
    externalHelpViewerButton: TButton;
    tipsRepo: TdxScreenTipRepository;
    cxHintStyleController1: TcxHintStyleController;
    procedure externalHelpViewerButtonClick(Sender: TObject);
    procedure internalHelpViewerButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest1: TFormTest1;

implementation

{$R *.dfm}

uses SampleFormStubsHelper, SampleOnly.Help.Hint.Setup;

procedure TFormTest1.externalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormStubsHelper.openExternalHelp;
end;

procedure TFormTest1.FormCreate(Sender: TObject);
begin
  TOPPClientHintHelper.LoadHints(self, '', self.cxHintStyleController1, self.tipsRepo,
    procedure()
    begin
    end);
end;

procedure TFormTest1.internalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormStubsHelper.openInternalHelp;
end;

end.
