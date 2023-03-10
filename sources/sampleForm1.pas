unit sampleForm1;

interface

  uses
    Vcl.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs;

  type
    TSampleForm = class(TForm)
      Button1: TButton;
      CheckBox1: TCheckBox;
      Edit1: TEdit;
      procedure Button1Click(Sender: TObject);
    private
      { Private declarations }
    public
      { Public declarations }
    end;

  var
    SampleForm: TSampleForm;
    oppHintPackage: THandle;

implementation

  uses
    HintBasics;

{$R *.dfm}
{$IFDEF DEBUG}
{$C +}
{$ENDIF}

  procedure TSampleForm.Button1Click(Sender: TObject);
  type
    SampleFormCall = procedure(userInfo: TOPPHintUserInfo); stdcall;
  var
    sampleFormCallProcedure: SampleFormCall;
    userInfo: TOPPHintUserInfo;
  begin
    @sampleFormCallProcedure := GetProcAddress(oppHintPackage, 'showHintDialog');
    if @sampleFormCallProcedure <> nil then begin
      userInfo.hintIdentifier := 997;
      userInfo.hintText := 'In nomine lorem ipsum';
      userInfo.hintPdfFilename := 'D:\GulfStream\Документация\ГОЛЬФСТРИМ\Руководства пользователя\гольфстрим_руководство пользователя.pdf';
      sampleFormCallProcedure(userInfo);
    end else begin
      ShowMessage('testFunc not found');
    end;

  end;

initialization

  oppHintPackage := LoadPackage('OPPHint.bpl');

finalization

  UnloadPackage(oppHintPackage);

end.
