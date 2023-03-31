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
    procedure CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>);
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
        procedure(hints: TList<TOPPHelpHint>)
        begin
          self.CreateHintViews(AForm, hints);
        end);
    finally
      fRequest.Free;
    end;
  finally
    fMetaFactory.Free;
  end;
end;

procedure TOPPClientHintHelper.SaveHints(AForm: TControl;useGlobal: Boolean;  AFilename: String; predicateFileName: String);
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

procedure TOPPClientHintHelper.CreateHintViews(AForm: TControl; hints: TList<TOPPHelpHint>);
var
  fHint: TOPPHelpHint;
  fControl: TControl;
  fScreenTip: TdxScreenTip;
  fScreenTipLink: TdxScreenTipLink;
begin

  if not assigned(fHintController) then
  begin
    eventLogger.Log(Format('%s not assigned', ['HintController']));
    exit;
  end;

  if not assigned(fRepo) then
  begin
    eventLogger.Log(Format('%s not assigned', ['ScreenRepo']));
    exit;
  end;

  eventLogger.Log(Format('will create screentips [%d]', [hints.Count]));
  for fHint in hints do
  begin

    fControl := AForm.FindSubControl(fHint.Meta);
    if not assigned(fControl) then
      exit;

    fControl.ShowHint := true;

    fScreenTip := fRepo.Items.Add;
    fScreenTip.Width := 789;

    fScreenTip.Header.PlainText := true;
    fScreenTip.Header.Text := ''; // Заголовок

    fScreenTip.Description.PlainText := false;
    fScreenTip.Description.Text := fHint.Data.Text; // rtf;

    //TdxScreenTipStyle.Create(self).ScreenTipLinks.Add;
    fScreenTipLink := TdxScreenTipStyle(fHintController.HintStyle).ScreenTipLinks.Add;
    fScreenTipLink.ScreenTip := fScreenTip;
    fScreenTipLink.control := fControl;

  end;
end;

end.
