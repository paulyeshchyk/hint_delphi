unit OPPHelpPreviewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, OPP.Help.View.Fullscreen;

type

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: Longint;
  end;

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    OPPHelpViewFullScreen1: TOPPHelpViewFullScreen;
  private
    { Private declarations }
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses OPP.Help.System.Messaging,
  OPP.Help.System.Stream,
  OPP.Help.Events;

procedure TForm1.WMCopyData(var Msg: TWMCopyData);
var
  res1: TOPPHelpViewFullScreenSharedMessage;
  Stream: TReadOnlyMemoryStream;
begin

  Stream := TReadOnlyMemoryStream.Create(Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
  res1.readFromStream(Stream);

  ShowMessage('Received correct message page:' + IntToStr(res1.page));

end;

end.
