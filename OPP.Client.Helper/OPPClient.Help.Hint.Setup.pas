unit OPPClient.Help.Hint.Setup;

interface

uses
  System.Classes,
  Vcl.Controls, System.Generics.Collections, System.SysUtils,
  dxScreenTip, cxHint,
  OPP.Help.Hint, OPP.Help.Meta;

type
  TOPPClientHintHelper = class
  private
    fHintController: TcxHintStyleController;
    fRepo: TdxScreenTipRepository;
    procedure CreateHintViews(AForm: TControl; AHintList: TList<TOPPHelpHint>);
    procedure AddDXScreenTip(AComponent: TComponent; AHint: TOPPHelpHint);
    procedure RecursiveApplyHint(AComponent: TComponent; AHint: TOPPHelpHint);
  public
    constructor Create(AHintController: TcxHintStyleController; ARepo: TdxScreenTipRepository);
    destructor Destroy; override;
    procedure LoadHints(AForm: TControl; AFilename: String = '');
    procedure SaveHints(AForm: TControl; useGlobal: Boolean; AFilename: String; predicateFileName: String);
  end;

implementation

uses
  OPP.Help.Log,
  OPP.Help.Component.Enumerator,

  OPP.Help.Hint.Server,
  OPPClient.Help.Meta.Factory;

constructor TOPPClientHintHelper.Create(AHintController: TcxHintStyleController; ARepo: TdxScreenTipRepository);
begin
  fHintController := AHintController;
  fRepo := ARepo;
end;

destructor TOPPClientHintHelper.Destroy;
begin
  fHintController := nil;
  fRepo := nil;
  inherited Destroy;
end;

procedure TOPPClientHintHelper.LoadHints(AForm: TControl; AFilename: String);
var
  fMetaFactory: TOPPHelpMetaHintFactory;
  fRequest: TOPPHelpHintMappingLoadRequest;
  fFileName: String;
begin
  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  try
    fRequest := TOPPHelpHintMappingLoadRequest.Create(AForm, AFilename);
    try
      //
      fRequest.OnGetHintFactory := function(AComponent: TComponent): TList<TOPPHelpMeta>
        begin
          result := fMetaFactory.GetChildrenHelpMeta(AComponent)
        end;

      helpHintServer.LoadHints(fRequest,
        procedure(HintTexts: TList<TOPPHelpHint>)
        begin
          self.CreateHintViews(AForm, HintTexts);
        end);
    finally
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

procedure TOPPClientHintHelper.SaveHints(AForm: TControl; useGlobal: Boolean; AFilename: String; predicateFileName: String);
var
  fRequest: TOPPHelpHintMappingSaveRequest;
  fMetaFactory: TOPPHelpMetaHintFactory;
begin

  fMetaFactory := TOPPHelpMetaHintFactory.Create;
  try
    fRequest := TOPPHelpHintMappingSaveRequest.Create(AForm, AFilename);
    try
      fRequest.DefaultPredicateFileName := predicateFileName;
      fRequest.OnGetHintFactory := function(AComponent: TComponent): TList<TOPPHelpMeta>
        begin
          result := fMetaFactory.GetChildrenHelpMeta(AComponent)
        end;

      helpHintServer.SaveHints(fRequest, useGlobal, nil);

    finally
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; AHintList: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fFormName: String;
begin
  fFormName := AForm.ClassName;
  if (not assigned(AHintList)) or (AHintList.Count = 0) then
  begin
    eventLogger.Warning(Format('Form: %s; No Screentip be created, because hints are not available', [fFormName]));
    exit;
  end;

  if not assigned(fHintController) then
  begin
    eventLogger.Warning(Format('Form: %s; No Screentip be created, because HintController is not defined', [fFormName]));
    exit;
  end;

  if not assigned(fRepo) then
  begin
    eventLogger.Warning(Format('Form: %s; No Screentip be created, because Tips repository is not defined', [fFormName]));
    exit;
  end;

  AForm.GetChildrenRecursive(nil,
    procedure(AComponent: TComponent)
    var
      fHint: TOPPHelpHint;
      fIsHintSupported: Boolean;
    begin
      for fHint in AHintList do
      begin
        fIsHintSupported := AComponent.isSupportingMeta(fHint.Meta);
        if fIsHintSupported then
        begin
          RecursiveApplyHint(AComponent, fHint);
          AddDXScreenTip(AComponent, fHint);
          break;
        end;
      end;
    end);
end;

procedure TOPPClientHintHelper.RecursiveApplyHint(AComponent: TComponent; AHint: TOPPHelpHint);
var
  fHintedControlChildren, fChildren: TList<TComponent>;
  fHintedChild: TComponent;
begin
  if not AComponent.canApplyHintsRecursively() then
    exit;

  fHintedControlChildren := AComponent.GetChildrenRecursive(nil, nil);
  for fHintedChild in fHintedControlChildren do
  begin
    AddDXScreenTip(fHintedChild, AHint);
  end;
end;

procedure TOPPClientHintHelper.AddDXScreenTip(AComponent: TComponent; AHint: TOPPHelpHint);
var
  fControl: TControl;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;

begin
  if not(AComponent is TControl) then
    exit;
  fControl := AComponent as TControl;

  fControl.ShowHint := true;
  fScreenTip := fRepo.Items.Add;
  fScreenTip.Width := 789;

  fScreenTip.Header.PlainText := true;
  fScreenTip.Header.Text := ''; // Заголовок

  fScreenTip.Description.PlainText := false;
  fScreenTip.Description.Text := AHint.Data.rtf; // rtf;

  fScreenTipLink := TdxScreenTipStyle(fHintController.HintStyle).ScreenTipLinks.Add;
  fScreenTipLink.ScreenTip := fScreenTip;
  fScreenTipLink.control := fControl;

end;

end.
