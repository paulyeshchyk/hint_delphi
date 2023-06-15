unit OPP.Info.Extractor.ClientOnly;

interface

uses
  Vcl.Controls,
  OPP.Buffer.OPPInfo,
  OPP.Buffer.Manager,
  OPP.Buffer.Manager.DatasetRecord;

type
  TWinControlOPPInfoExtractor = class(TOPPInfoExtractor)
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo; override;
    function isApplicable(Sender: TWinControl):Boolean;override;
    procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl); override;
  end;

  TOPPObjControlOPPInfoExtractor = class(TOPPInfoExtractor)
  public
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo; override;
    function isApplicable(Sender: TWinControl):Boolean;override;
    procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl); override;
  end;

  TOPPAttrControlOPPInfoExtractor = class(TOPPInfoExtractor)
  public
    function GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo; override;
    function isApplicable(Sender: TWinControl):Boolean;override;
    procedure SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl); override;
  end;

implementation

uses
  System.SysUtils, System.Variants,

  OPP.Help.Component.Enumerator,
  OPP.Help.System.Control,
  OppAttrControl,
  OppObjControl;

type
  TOPPBufferObjControlHelper = class helper for TOppObjControl
  private
    function GetLoodsmanType: String;
  public
    function OPPInfo: TOPPBufferOPPInfo;
    function isTypeAcceptable(AType: String): Boolean;
    property loodsmanType: String read GetLoodsmanType;
  end;

  TOPPBufferAttrControlHelper = class helper for TOppAttrControl
    function OPPInfo: TOPPBufferOPPInfo;
  end;

  TOPPBufferWinControlHelper = class helper for TWinControl
    function OPPInfo: TOPPBufferOPPInfo;
  end;

  { TOPPBufferObjControlHelper }

function TOPPBufferObjControlHelper.GetLoodsmanType: String;
begin
  result := self.TypeObject;
end;

function TOPPBufferObjControlHelper.isTypeAcceptable(AType: String): Boolean;
begin
  result := (CompareStr(Uppercase(self.loodsmanType), Uppercase(AType)) = 0);
end;

function TOPPBufferObjControlHelper.OPPInfo: TOPPBufferOPPInfo;
begin
  result := TOPPBufferOPPInfo.Create(otObjControl);
  result.loodsmanType := self.loodsmanType;
  result.loodsmanId := Format('%d', [self.ObjectID]);
  result.loodsmanAttribute := self.Attribute;
  if Assigned(FControl) then
    result.ControlText := VarToStr(FControl.EditValue);
end;

{ TOPPBufferAttrControlHelper }

function TOPPBufferAttrControlHelper.OPPInfo: TOPPBufferOPPInfo;
begin
  result := TOPPBufferOPPInfo.Create(otAttrControl);
  result.loodsmanType := '';
  result.loodsmanId := self.DataInControl;
  result.loodsmanAttribute := self.Attribute;
  if Assigned(FControl) then
    result.ControlText := VarToStr(FControl.EditValue);
end;

{ TOPPBufferWinControlHelper }

function TOPPBufferWinControlHelper.OPPInfo: TOPPBufferOPPInfo;
begin
  result := nil;
  if not(self is TWinControl) then
    exit;

  result := TOPPBufferOPPInfo.Create(otWinControl);
  result.ControlText := self.TextPropertyValue;
end;

{ TOPPAttrControlInfoExtractor }

function TOPPAttrControlOPPInfoExtractor.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
var
  foppAttrControl: TOppAttrControl;
begin
  result := nil;
  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(Sender);
  if not(foppAttrControl is TOppAttrControl) then
    exit;
  result := TOPPBufferOPPInfo.Create(otAttrControl);
  result.loodsmanType := '';
  result.loodsmanId := foppAttrControl.DataInControl;
  result.loodsmanAttribute := foppAttrControl.Attribute;
  if Assigned(foppAttrControl.FControl) then
    result.ControlText := VarToStr(foppAttrControl.FControl.EditValue);
end;

function TOPPAttrControlOPPInfoExtractor.isApplicable(Sender: TWinControl): Boolean;
var foppAttrControl:TOppAttrControl;
begin
  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(Sender);
  result := Assigned(fOPPAttrControl);
end;

procedure TOPPAttrControlOPPInfoExtractor.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
var
  foppAttrControl: TOppAttrControl;
begin
  if (not(AControl is TWinControl)) or (not Assigned(OPPInfo)) then
    exit;

  foppAttrControl := TOPPHelpWinControlExtractor<TOppAttrControl>.GetParent(AControl);
  if not Assigned(foppAttrControl) then
    exit;

  if OPPInfo.oppBufferType = otAttrControl then
  begin
    foppAttrControl.DataInControl := OPPInfo.loodsmanId;
  end else begin
    foppAttrControl.DataInControl := AText;
  end;
end;

{ TOPPObjControlInfoExtractor }

function TOPPObjControlOPPInfoExtractor.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
var
  foppObjControl: TOppObjControl;
begin
  result := nil;
  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(Sender);
  if not(foppObjControl is TOppObjControl) then
    exit;
  result := TOPPBufferOPPInfo.Create(otObjControl);
  result.loodsmanType := foppObjControl.loodsmanType;
  result.loodsmanId := Format('%d', [foppObjControl.ObjectID]);
  result.loodsmanAttribute := foppObjControl.Attribute;
  if Assigned(foppObjControl.FControl) then
    result.ControlText := VarToStr(foppObjControl.FControl.EditValue);
end;

function TOPPObjControlOPPInfoExtractor.isApplicable(Sender: TWinControl): Boolean;
begin
  result := false;
  if (not(Sender is TWinControl))then
    exit;
  result := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(Sender) <> nil;
end;

procedure TOPPObjControlOPPInfoExtractor.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
var
  foppObjControl: TOppObjControl;
begin
  if (not(AControl is TWinControl)) or (not Assigned(OPPInfo)) then
    exit;
  foppObjControl := TOPPHelpWinControlExtractor<TOppObjControl>.GetParent(AControl);

  if Assigned(foppObjControl) then
  begin
    if (CompareStr(Uppercase(OPPInfo.loodsmanType), Uppercase(foppObjControl.TypeObject)) = 0) then
    begin
      try
        foppObjControl.ObjectID := StrToInt(OPPInfo.loodsmanId);
      except
        foppObjControl.ObjectID := 0;
      end;
    end else begin
      foppObjControl.DataInControl := AText;
    end;
    exit;
  end;
end;

{ TWinControlOPPInfoExtractor }

function TWinControlOPPInfoExtractor.GetOPPInfo(Sender: TWinControl): TOPPBufferOPPInfo;
begin
  result := nil;

  //если sender == nil тогда текст пришёл из внешнего источника
  //иначе sender должен быть TWinControl
  if (not Assigned(Sender)) or (Sender is TWinControl) then begin
    result := TOPPBufferOPPInfo.Create(otWinControl);
    if Sender.TextSelectionLength <> 0 then begin
      result.ControlText := Sender.TextSelectionPropertyValue;
    end else begin
      result.ControlText := Sender.TextPropertyValue;
    end;
  end;

end;

function TWinControlOPPInfoExtractor.isApplicable(Sender: TWinControl): Boolean;
begin
  result := (Sender is TWinControl);
end;

procedure TWinControlOPPInfoExtractor.SetOPPInfo(OPPInfo: TOPPBufferOPPInfo; AText: String; AControl: TWinControl);
begin
  if (not(AControl is TWinControl)) or (not Assigned(OPPInfo)) then
    exit;
  AControl.SetTextPropertyValue(AText);
end;

end.
