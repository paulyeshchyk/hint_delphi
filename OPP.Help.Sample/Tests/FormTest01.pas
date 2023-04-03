unit FormTest01;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormTest1 = class(TForm)
    GroupBox11: TGroupBox;
    Kod_OKWED: TCheckBox;
    Kod_MKC: TEdit;
    internalHelpViewerButton: TButton;
    externalHelpViewerButton: TButton;
    procedure externalHelpViewerButtonClick(Sender: TObject);
    procedure internalHelpViewerButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest1: TFormTest1;

implementation

{$R *.dfm}

uses SampleFormHelper;

procedure TFormTest1.externalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormHelper.openExternalHelp;
end;

procedure TFormTest1.internalHelpViewerButtonClick(Sender: TObject);
begin
  TSampleFormHelper.openInternalHelp;
end;

end.
