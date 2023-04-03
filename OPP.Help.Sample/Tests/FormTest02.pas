unit FormTest02;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormTest2 = class(TForm)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    generateHintMappingButton: TButton;
    procedure generateHintMappingButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest2: TFormTest2;

implementation

{$R *.dfm}

uses SampleFormHelper;

procedure TFormTest2.generateHintMappingButtonClick(Sender: TObject);
begin
  TSampleFormHelper.generateHintMapping;
end;

end.
