unit OPP.Help.VCL.PanelTrigger;

interface

uses
  System.Classes,
  System.Generics.Collections,
  VCL.Menus, dxDockControl, dxDockPanel;

type
  TOPPHelpVCLPanelTriggerAction = reference to procedure(AVisible: Boolean);

  TOPPHelpVCLPanelTrigger = class
  private
    fTitle: String;
    fIsChecked: Boolean;
    fOnAction: TOPPHelpVCLPanelTriggerAction;
    fMenuItem: TMenuItem;
    fEnabled: Boolean;
    flastKnownCheckedState: Boolean;
    procedure SetIsChecked(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure turnOff;
    procedure turnOn;
    property lastKnownCheckedState: Boolean read flastKnownCheckedState write flastKnownCheckedState default true;
  public
    procedure AddMenuItem(AContainer: TMenuItem);
    procedure OnClick(Sender: TObject);
    procedure OnActionExternal(Sender: TdxCustomDockControl);
    property IsChecked: Boolean read fIsChecked write SetIsChecked default False;
    property Title: String read fTitle write fTitle;
    property OnAction: TOPPHelpVCLPanelTriggerAction read fOnAction write fOnAction;
    property Enabled: Boolean read fEnabled write SetEnabled default true;
  end;

  TOPPHelpVCLPanelTriggerContainer = class
  private
    fList: TObjectList<TOPPHelpVCLPanelTrigger>;
    fMenuContainer: TMenuItem;
  public
    constructor Create(AMenuContainer: TMenuItem);
    destructor Destroy; override;
    function AddTrigger(APanel: TdxDockPanel): TOPPHelpVCLPanelTrigger;
  end;

implementation

{ TOPPHelpVCLPanelTriggerContainer }

function TOPPHelpVCLPanelTriggerContainer.AddTrigger(APanel: TdxDockPanel): TOPPHelpVCLPanelTrigger;
var
  fTrigger: TOPPHelpVCLPanelTrigger;
begin
  if not Assigned(APanel) then
    exit;

  fTrigger := TOPPHelpVCLPanelTrigger.Create;
  fTrigger.Title := APanel.Caption;
  fTrigger.IsChecked := APanel.Visible;
  fTrigger.OnAction := procedure(AIsVisible: Boolean)
    begin
      if AIsVisible then
        APanel.Show
      else
        APanel.Hide;
    end;
  APanel.OnVisibleChanged := fTrigger.OnActionExternal;

  fList.Add(fTrigger);
  fTrigger.AddMenuItem(fMenuContainer);
  result := fTrigger;
end;

constructor TOPPHelpVCLPanelTriggerContainer.Create(AMenuContainer: TMenuItem);
begin
  fList := TObjectList<TOPPHelpVCLPanelTrigger>.Create;
  fMenuContainer := AMenuContainer;
end;

destructor TOPPHelpVCLPanelTriggerContainer.Destroy;
begin
  fList.Clear;
  fList.Free;
  inherited;
end;

{ TOPPHelpVCLPanelTrigger }

procedure TOPPHelpVCLPanelTrigger.AddMenuItem(AContainer: TMenuItem);
begin
  if not Assigned(AContainer) then
    exit;
  if Assigned(fMenuItem) then
  begin
    AContainer.Add(fMenuItem);
    exit;
  end;

  fMenuItem := TMenuItem.Create(AContainer);
  fMenuItem.Caption := self.Title;
  fMenuItem.OnClick := self.OnClick;
  fMenuItem.Checked := self.IsChecked;
  fMenuItem.Enabled := true;//self.Enabled;
  AContainer.Add(fMenuItem);
end;

procedure TOPPHelpVCLPanelTrigger.turnOff;
begin
//  self.lastKnownCheckedState := self.isChecked;
//  self.IsChecked := False;
//  if Assigned(fMenuItem) then
//    fMenuItem.Enabled := False;
//  if Assigned(fOnAction) then
//    fOnAction(self.IsChecked);
end;

procedure TOPPHelpVCLPanelTrigger.turnOn;
begin
//  if Assigned(fMenuItem) then
//    fMenuItem.Enabled := true;
//  self.isChecked := self.lastKnownCheckedState;
//  if Assigned(fOnAction) then
//    fOnAction(self.IsChecked);
end;

procedure TOPPHelpVCLPanelTrigger.OnActionExternal(Sender: TdxCustomDockControl);
begin
  self.IsChecked := Sender.Visible;
end;

procedure TOPPHelpVCLPanelTrigger.OnClick(Sender: TObject);
begin
  self.IsChecked := not self.IsChecked;

  if Sender is TMenuItem then
  begin
    TMenuItem(Sender).Checked := self.IsChecked;
  end;

  if Assigned(fOnAction) then
    fOnAction(self.IsChecked);
end;

procedure TOPPHelpVCLPanelTrigger.SetEnabled(const Value: Boolean);
begin
  fEnabled := Value;
  if Assigned(fMenuItem) then
    fMenuItem.Enabled := fEnabled;
  if fEnabled = False then
  begin
    turnOff;
  end else begin
    turnOn;
  end;
end;

procedure TOPPHelpVCLPanelTrigger.SetIsChecked(const Value: Boolean);
begin
  fIsChecked := Value;
  if Assigned(fMenuItem) then
    fMenuItem.Checked := fIsChecked;
end;

end.
