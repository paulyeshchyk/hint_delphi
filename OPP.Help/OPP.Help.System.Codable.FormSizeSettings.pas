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
    function GetFormSettings: TOPPHelpSystemCodableFormSizeSettings;
    procedure SetFormSettings(const Value: TOPPHelpSystemCodableFormSizeSettings);
  public
    procedure ReadFormState();
    procedure SaveFormState();

    property SizeSettingFileName: String read GetSizeSettingFileName;
    property Frame: TRect read GetFrame write SetFrame;
    property FormSettings: TOPPHelpSystemCodableFormSizeSettings read GetFormSettings write SetFormSettings;
  end;

implementation

uses
  System.SysUtils,
  OPP.Help.Log,
  OPP.Help.System.Codable.Helper;

{ TFormSizeHelper }

function TFormSizeHelper.GetFormSettings: TOPPHelpSystemCodableFormSizeSettings;
var
  fResult: TOPPHelpSystemCodableFormSizeSettings;
begin
  fResult := TOPPHelpSystemCodableFormSizeSettings.Create;
  fResult.Frame := self.Frame;
  fResult.WindowState := self.WindowState;
  result := fResult;
end;

function TFormSizeHelper.GetFrame: TRect;
begin
  result.Left := self.Left;
  result.Top := self.Top;
  result.Right := self.Left + self.Width;
  result.Bottom := self.Top + self.Height;
end;

function TFormSizeHelper.GetSizeSettingFileName: String;
begin
  result := self.ClassName + 'FormSize.settings';
end;

procedure TFormSizeHelper.ReadFormState;
var
  fResult: TOPPHelpSystemCodableFormSizeSettings;
begin
  try
    TOPPCodableHelper<TOPPHelpSystemCodableFormSizeSettings>.Decode(SizeSettingFileName, fResult);
    try
      self.FormSettings := fResult;
    finally
      fResult.free;
    end;
  except
    on E: Exception do
    begin
      eventLogger.Error(E, 'TForm');
    end;
  end;
end;

procedure TFormSizeHelper.SaveFormState;
var
  fResult: TOPPHelpSystemCodableFormSizeSettings;
begin
  fResult := self.FormSettings;
  try
    try
      TOPPCodableHelper<TOPPHelpSystemCodableFormSizeSettings>.Encode(SizeSettingFileName, fResult);
    except
      on E: Exception do
      begin
        eventLogger.Error(E, 'TForm');
      end;
    end;
  finally
    fResult.free;
  end;
end;

procedure TFormSizeHelper.SetFormSettings(const Value: TOPPHelpSystemCodableFormSizeSettings);
begin
  if not Assigned(Value) then exit;
  self.Frame := Value.Frame;
  self.WindowState := Value.WindowState;
end;

procedure TFormSizeHelper.SetFrame(const Value: TRect);
begin
  self.Left := Value.Left;
  self.Top := Value.Top;
  self.Width := Value.Right - Value.Left;
  self.Height := Value.Bottom - Value.Top;
end;

{ TOPPHelpSystemCodableFormSizeSettings }

procedure TOPPHelpSystemCodableFormSizeSettings.SetDefaults;
begin
  inherited;
  self.Frame := Screen.WorkAreaRect;
  self.WindowState := wsNormal;
end;

end.
