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
    procedure CreateHintViews(AForm: TControl; AHintTexts: TList<TOPPHelpHint>);
    procedure ApplyHint(AComponent: TComponent; AHint: TOPPHelpHint);
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

procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; AHintTexts: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TControl;
  fChild: TComponent;
  fChildren: TList<TComponent>;
begin

  if (not assigned(AHintTexts)) or (AHintTexts.Count = 0) then
  begin
    eventLogger.Warning(Format('No Screentip be created, because hints are not available for [%s]', [AForm.ClassName]));
    exit;
  end;

  if not assigned(fHintController) then
  begin
    eventLogger.Warning(Format('No Screentip be created, because HintController is not defined in [%s]', [AForm.ClassName]));
    exit;
  end;

  if not assigned(fRepo) then
  begin
    eventLogger.Warning(Format('No Screentip be created, because Tips repository is not defined in [%s]', [AForm.ClassName]));
    exit;
  end;

  fChildren := AForm.GetChildrenRecursive(nil);

  eventLogger.Debug(Format('will create [%d] hints for [%s]', [AHintTexts.Count, AForm.ClassName]));
  for fHint in AHintTexts do
  begin

    eventLogger.Debug(Format('looking for control with hint.id [%s]', [fHint.Meta.identifier]));

    fControl := nil;
    for fChild in fChildren do
    begin
      if fChild.isSupportingMeta(fHint.Meta) then
      begin
        fControl := fChild as TControl;
        break;
      end;
    end;

    if not assigned(fControl) then
    begin
      eventLogger.Debug(Format('will not create hint window for hint.id [%s], because form [%s] has no component supporting that', [fHint.Meta.identifier, AForm.ClassName]));
      continue;
    end;

    eventLogger.Debug(Format('will create hint window for hint.id [%s] with text', [fHint.Meta.identifier, fHint.Data.rtf]));

    RecursiveApplyHint(fControl, fHint);

    ApplyHint(fControl, fHint);

  end;
end;

procedure TOPPClientHintHelper.RecursiveApplyHint(AComponent: TComponent; AHint: TOPPHelpHint);
var
  fHintedControlChildren, fChildren: TList<TComponent>;
  fHintedChild: TComponent;
begin
  fHintedControlChildren := AComponent.GetChildrenRecursive(nil);
  for fHintedChild in fHintedControlChildren do
  begin
    ApplyHint(fHintedChild, AHint);
  end;
end;

procedure TOPPClientHintHelper.ApplyHint(AComponent: TComponent; AHint: TOPPHelpHint);
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
