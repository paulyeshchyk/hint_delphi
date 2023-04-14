unit OPPClient.TdxScreenTip.Helper;

interface

uses
  dxScreenTip;

type
  TOPPClientTdxScreenTipHelper = class helper for TdxScreenTip
    procedure setAspectRatio(AValue: Double; rtf: String; maxWidthValue: Integer = 0);
  end;

implementation

uses
  System.Types, Vcl.Graphics, WinAPI.Windows,
  System.Math,
  cxGeometry, dxDrawRichTextUtils;

{ TOPPClientTdxScreenTipHelper }

procedure TOPPClientTdxScreenTipHelper.setAspectRatio(AValue: Double; rtf: String; maxWidthValue: Integer);
var
  fCanvas: TCanvas;
  fScaleFactor: TdxScaleFactor;
  fRect: TRect;
  fHelper: TdxRichTextHelper;
  calculatedAspectRatio: Double;
  result: Integer;
const
  minimumWidth: Integer = 12;

begin
  fRect := TRect.Empty;
  fRect.Width := minimumWidth;

  fCanvas := TCanvas.Create;
  try
    fCanvas.Handle := GetDC(0);
    fScaleFactor := TdxScaleFactor.Create;
    try

      fHelper := TdxRichTextHelper.Create;
      try
        fHelper.Init(fCanvas, rtf, fScaleFactor);
        fHelper.CalculateTextHeight(fCanvas, fRect);

        calculatedAspectRatio := (fRect.Height / fRect.Width);
        if calculatedAspectRatio > AValue then
        begin
          result := round((fRect.height / 2) / AValue);
        end else begin
          result := fRect.Width;
        end;

        self.Width := Max(maxWidthValue, result);

      finally
        fHelper.Free;
      end;
    finally
      ReleaseDC(0, fCanvas.Handle);
      fScaleFactor.Free;
    end;
  finally
    fCanvas.Free;
  end;
end;

end.
