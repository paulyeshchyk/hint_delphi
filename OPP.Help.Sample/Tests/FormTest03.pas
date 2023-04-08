unit FormTest03;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormTest3 = class(TForm)
    GroupBox3: TGroupBox;
    Button1: TButton;
    Memo1: TMemo;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTest3: TFormTest3;

implementation

{$R *.dfm}

uses SampleFormStubsHelper;

procedure TFormTest3.Button1Click(Sender: TObject);
begin
  TSampleFormStubsHelper.savePredicateToStream;
end;

procedure TFormTest3.Button2Click(Sender: TObject);
begin
  TSampleFormStubsHelper.savePredicateToFile;
end;

procedure TFormTest3.Button5Click(Sender: TObject);
begin
  TSampleFormStubsHelper.readPredicateFromFile;
end;

end.
