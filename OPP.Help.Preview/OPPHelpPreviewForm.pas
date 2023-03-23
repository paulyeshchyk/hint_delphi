unit OPPHelpPreviewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, OPP.Help.View.Fullscreen;

type

  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

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
    procedure WMUserTen(var Msg: TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WMUserTen(var Msg: TWMCopyData);
var
  copyDataType: TCopyDataType;
  res1, data1: TOPPHelpViewFullScreenSharedMessage;
begin
  copyDataType := TCopyDataType(Msg.CopyDataStruct.dwData);
  res1 := data1.unpack(Msg.CopyDataStruct);

//  if assigned(pData) then
//  begin
//    res1 := data1.unpack(Msg.CopyDataStruct);
//    ShowMessage('Received correct message page:' + IntToStr(res1.page));
//  end else begin
//    ShowMessage('Received wrong message');
//  end;
end;

end.
