unit OPP.Help.System.Codable.FormSizeSettings;

interface

uses
  Vcl.Forms, System.Types,
  OPP.Help.System.Codable;

type
  TOPPHelpSystemCodableFormSizeSettings = class(TOPPCodable)
  private
    fWindowState: TWindowState;
    fFrame: TRect;
  public
    procedure SetDefaults(); override;
    property WindowState: TWindowState read fWindowState write fWindowState;
    property Frame: TRect read fFrame write fFrame;
  end;

  TFormSizeHelper = class helper for TForm
  private
    function GetFrame: TRect;
    procedure SetFrame(const Value: TRect);
    function GetSizeSettingFileName: String;
  public
    procedure ReadFormState();
    procedure SaveFormState();

    property SizeSettingFileName: String read GetSizeSettingFileName;
    property frame: TRect read GetFrame write SetFrame;
  end;

implementation

uses
  System.SysUtils,
  OPP.Help.Log,
  OPP.Help.System.Codable.Helper;

{ TFormSizeHelper }

function TFormSizeHelper.GetFrame: TRect;
begin
  result.Left := self.Left;
  result.Top := self.Top;
  result.Right := self.Left + self.Width;
  result.Bottom := self.Top + self.Height;
end;

function TFormSizeHelper.GetSizeSettingFileName: String;
begin
  result := Self.ClassName + 'FormSize.settings';
end;

procedure TFormSizeHelper.ReadFormState;
var
  fResult: TOPPHelpSystemCodableFormSizeSettings;
begin
  try
    TOPPCodableHelper<TOPPHelpSystemCodableFormSizeSettings>.Decode(SizeSettingFileName, fResult);
    self.frame := fResult.Frame;
    self.WindowState := fResult.WindowState;
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

procedure TFormSizeHelper.SaveFormState;
var
  fResult: TOPPHelpSystemCodableFormSizeSettings;
begin
  fResult := TOPPHelpSystemCodableFormSizeSettings.Create;
  try
    fResult.Frame := self.frame;
    fResult.WindowState := self.WindowState;
    try
      TOPPCodableHelper<TOPPHelpSystemCodableFormSizeSettings>.Encode(SizeSettingFileName, fResult);
    except
      on E: Exception do
      begin
        eventLogger.Error(E);
      end;
    end;
  finally
    fResult.Free;
  end;
end;

procedure TFormSizeHelper.SetFrame(const Value: TRect);
begin
  self.Left := value.Left;
  self.Top := value.Top;
  self.Width := value.Right - value.Left;
  self.Height := value.Bottom - value.Top;
end;

{ TOPPHelpSystemCodableFormSizeSettings }

procedure TOPPHelpSystemCodableFormSizeSettings.SetDefaults;
begin
  inherited;
  self.Frame := TRect.Create(0, 0, 500, 200);
  self.WindowState := wsNormal;
end;

end.
