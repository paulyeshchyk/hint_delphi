unit HintDialog;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls,

    dxPDFDocument, dxPDFCore,

    HintBasics, HintReader;

  type
    TOPPHintDialog = class(TForm)
      Label1: TLabel;
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure FormCreate(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
      { Private declarations }
    var
      HintReader: TOPPHintReader;
      fUserInfo: TOPPHintUserInfo;
      fDialogCompletion: TOPPHintDialogCompletion;

      procedure SetUserInfo(const userInfo: TOPPHintUserInfo);
      procedure SetDialogCompletion(completion: TOPPHintDialogCompletion);
      procedure onPDFLoaded(document: TdxPDFDocument);

    published
      property userInfo: TOPPHintUserInfo read fUserInfo write SetUserInfo;
      property dialogCompletion: TOPPHintDialogCompletion read fDialogCompletion write SetDialogCompletion;
    end;

implementation

{$R *.dfm}

  procedure TOPPHintDialog.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    HintReader.destroy;
  end;

  procedure TOPPHintDialog.SetDialogCompletion(completion: TOPPHintDialogCompletion);
  begin
    fDialogCompletion := completion;
  end;

  procedure TOPPHintDialog.SetUserInfo(const userInfo: TOPPHintUserInfo);
  begin
    fUserInfo := userInfo;
    hintReader := TOPPHintReader.create();
    hintReader.OnLoaded := onPDFLoaded;
    hintReader.fileName := userInfo.hintPdfFilename;
  end;

  procedure TOPPHintDialog.FormCreate(Sender: TObject);
  begin
    Label1.Caption := IntToStr(self.userInfo.hintIdentifier);
  end;

  procedure TOPPHintDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    self.fDialogCompletion(self);
  end;

  procedure TOPPHintDialog.onPDFLoaded(document: TdxPDFDocument);
  var
  pageInfo: TdxPDFPageInfo;
  info: TdxPDFDocumentInformation;
  begin
    pageInfo := document.PageInfo[666];
    Label1.Caption := pageInfo.Text;
  end;

end.
