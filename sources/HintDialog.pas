unit HintDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,

  HintBasics, HintReader,
  dxCore, dxPDFCore, dxPDFDocument, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxPDFBase, dxPDFText,
  dxPDFRecognizedObject, dxCustomPreview, dxPDFDocumentViewer, dxPDFViewer, cxContainer, cxEdit, cxTextEdit, cxMemo,
  cxRichEdit;

type

  TOPPHintDialog = class(TForm)
    dxPDFViewer1: TdxPDFViewer;
    cxRichEdit1: TcxRichEdit;
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
  HintReader := TOPPHintReader.create(dxPDFViewer1);
  HintReader.OnLoaded := onPDFLoaded;
  HintReader.fileName := userInfo.hintPdfFilename;
end;

procedure TOPPHintDialog.FormCreate(Sender: TObject);
begin
  // Label1.Caption := IntToStr(self.userInfo.hintIdentifier);
end;

procedure TOPPHintDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  self.fDialogCompletion(self);
end;

procedure TOPPHintDialog.onPDFLoaded(document: TdxPDFDocument);
var
  pageInfo: TdxPDFPageInfo;
  info: TdxPDFDocumentInformation;
  hyperLink: TdxPDFHyperlink;
begin
//  dxPDFViewer1.lo

//  pageInfo := document.pageInfo[2];
//  hyperLink := pageInfo.Hyperlinks[0];

  // Label1.Caption := hyperLink.Hint;
end;

end.
