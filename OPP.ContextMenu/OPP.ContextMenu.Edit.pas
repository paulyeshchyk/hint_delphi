unit OPP.ContextMenu.Edit;

interface

uses Vcl.Menus, System.Classes, WinAPI.Messages, WinAPI.Windows, Vcl.Controls,
  cxEdit;

type
  TOPPContextMenuItemOnValidate = reference to function(Sender: TWinControl): Boolean;

  TOPPContextMenuItem = class(TMenuItem)
  private
    fOnValidate: TOPPContextMenuItemOnValidate;
  public
    property OnValidate: TOPPContextMenuItemOnValidate read fOnValidate write fOnValidate;
  end;

  TOPPContextMenuEdit = class(TPopupMenu)
  private
    fcxEditRepositoryItem: TcxEditRepositoryItem;
  protected
    procedure Popup(X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; AIconRepositoryItem: TcxEditRepositoryItem);
    class function CreateCustomMenuItem(AOwner: TComponent; Caption: String; onClick: TNotifyEvent; OnValidate: TOPPContextMenuItemOnValidate): TOPPContextMenuItem;
  end;

  TOPPContextMenuEditHelper = class Helper for TOPPContextMenuEdit
  public
    class function GetPopupComponentAsEditControl(Sender: TObject): TWinControl;
    class function GetPopupComponentAsEditControlForAction(Sender: TObject): TWinControl;

    procedure OnCutText(Sender: TObject);
    procedure OnCopyText(Sender: TObject);
    procedure OnPasteText(Sender: TObject);
    procedure OnDeleteText(Sender: TObject);
    procedure OnSelectAllText(Sender: TObject);
    procedure OnOPPBufferOpen(Sender: TObject);

    function OnValidateCutText(Sender: TWinControl): Boolean;
    function OnValidateCopyText(Sender: TWinControl): Boolean;
    function OnValidatePasteText(Sender: TWinControl): Boolean;
    function OnValidateDeleteText(Sender: TWinControl): Boolean;
    function OnValidateSelectAllText(Sender: TWinControl): Boolean;
    function OnValidateOPPBufferOpen(Sender: TWinControl): Boolean;
  end;

implementation

uses
  System.RTTI,
  Vcl.ClipBrd, Vcl.Forms,

  OPP.Help.Log,
  OPP.Help.System.Control,
  OPP.Buffer.Form,
  OPP.Buffer.Clipboard;

{ TOPPContextMenuEdit }

constructor TOPPContextMenuEdit.Create(AOwner: TComponent; AIconRepositoryItem: TcxEditRepositoryItem);
begin
  inherited Create(AOwner);
  self.fcxEditRepositoryItem := AIconRepositoryItem;

  self.Items.Add(CreateCustomMenuItem(self, 'Вырезать', OnCutText, OnValidateCutText));
  self.Items.Add(CreateCustomMenuItem(self, 'Копировать', OnCopyText, OnValidateCopyText));
  self.Items.Add(CreateCustomMenuItem(self, 'Вставить', OnPasteText, OnValidatePasteText));
  self.Items.Add(CreateCustomMenuItem(self, 'Удалить', OnDeleteText, OnValidateDeleteText));
  self.Items.Add(CreateCustomMenuItem(self, '-', nil, nil));
  self.Items.Add(CreateCustomMenuItem(self, 'Выделить всё', OnSelectAllText, OnValidateSelectAllText));
  self.Items.Add(CreateCustomMenuItem(self, '-', nil, nil));
  self.Items.Add(CreateCustomMenuItem(self, 'Буфер обмена ГС', OnOPPBufferOpen, nil));
end;

class function TOPPContextMenuEdit.CreateCustomMenuItem(AOwner: TComponent; Caption: String; onClick: TNotifyEvent; OnValidate: TOPPContextMenuItemOnValidate): TOPPContextMenuItem;
begin
  result := TOPPContextMenuItem.Create(AOwner);
  result.Caption := Caption;
  result.onClick := onClick;
  result.OnValidate := OnValidate;
end;

procedure TOPPContextMenuEdit.Popup(X, Y: Integer);
var
  item: TMenuItem;
begin
  for item in self.Items do
  begin
    if item is TOPPContextMenuItem then
    begin
      if (self.PopupComponent is TWinControl) then
      begin
        if Assigned(TOPPContextMenuItem(item).OnValidate) then
          item.Enabled := TOPPContextMenuItem(item).OnValidate(TWinControl(self.PopupComponent))
        else
          item.Enabled := true;
      end else begin
        item.Enabled := false;
      end;
    end;
  end;

  inherited;
end;

{ TOPPContextMenuEditHelper }

class function TOPPContextMenuEditHelper.GetPopupComponentAsEditControl(Sender: TObject): TWinControl;
var
  PopupComponent: TWinControl;
begin
  result := nil;
  if not(Sender is TWinControl) then
    exit;
  PopupComponent := TWinControl(Sender);
  if PopupComponent is TcxCustomEdit then
  begin
    result := TcxCustomEdit(PopupComponent).InnerControl;
  end else begin
    result := PopupComponent;
  end
end;

class function TOPPContextMenuEditHelper.GetPopupComponentAsEditControlForAction(Sender: TObject): TWinControl;
var
  PopupComponent: TWinControl;
  fComponent: TComponent;
begin
  result := nil;

  if not(Sender is TOPPContextMenuItem) then
    exit;
  if not(TOPPContextMenuItem(Sender).Owner is TOPPContextMenuEdit) then
    exit;
  fComponent := TOPPContextMenuEdit(TOPPContextMenuItem(Sender).Owner).PopupComponent;
  if not Assigned(fComponent) then
    exit;
  if not(fComponent is TWinControl) then
    exit;
  PopupComponent := TWinControl(fComponent);
  if PopupComponent is TcxCustomEdit then
  begin
    result := TcxCustomEdit(PopupComponent).InnerControl;
  end else begin
    result := PopupComponent;
  end
end;

procedure TOPPContextMenuEditHelper.OnCopyText(Sender: TObject);
var
  PopupComponent: TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControlForAction(Sender);
  if Assigned(PopupComponent) and (PopupComponent is TWinControl) then
    SendMessage(TWinControl(PopupComponent).Handle, WM_COPY, 0, 0);
end;

procedure TOPPContextMenuEditHelper.OnCutText(Sender: TObject);
var
  PopupComponent: TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControlForAction(Sender);
  if Assigned(PopupComponent) and (PopupComponent is TWinControl) then
    SendMessage(TWinControl(PopupComponent).Handle, WM_CUT, 0, 0);
end;

procedure TOPPContextMenuEditHelper.OnDeleteText(Sender: TObject);
var
  PopupComponent: TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControlForAction(Sender);
  if Assigned(PopupComponent) and (PopupComponent is TWinControl) then
    SendMessage(TWinControl(PopupComponent).Handle, WM_CLEAR, 0, 0);
end;

procedure TOPPContextMenuEditHelper.OnOPPBufferOpen(Sender: TObject);
begin
  if FindWindow('TOPPBufferForm', nil) = 0 then
  begin
    TOPPBufferForm.ShowForm(nil, self.fcxEditRepositoryItem, Screen.ActiveControl);
  end
  else
    eventLogger.Debug('Cant run second instance');
end;

procedure TOPPContextMenuEditHelper.OnPasteText(Sender: TObject);
var
  PopupComponent: TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControlForAction(Sender);
  if Assigned(PopupComponent) and (PopupComponent is TWinControl) then
    SendMessage(TWinControl(PopupComponent).Handle, WM_PASTE, 0, 0);
end;

procedure TOPPContextMenuEditHelper.OnSelectAllText(Sender: TObject);
var
  PopupComponent: TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControlForAction(Sender);
  if Assigned(PopupComponent) and (PopupComponent is TWinControl) then
    SendMessage(TWinControl(PopupComponent).Handle, EM_SETSEL, 0, -1);
end;

function TOPPContextMenuEditHelper.OnValidateCopyText(Sender: TWinControl): Boolean;
var
  PopupComponent: TWinControl;
begin
  result := false;
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if not Assigned(PopupComponent) then
    exit;
  result := (PopupComponent.TextSelectionLength > 0);
end;

function TOPPContextMenuEditHelper.OnValidateCutText(Sender: TWinControl): Boolean;
var
  PopupComponent: TWinControl;
begin
  result := false;
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if not Assigned(PopupComponent) then
    exit;
  result := (PopupComponent.TextSelectionLength > 0);
end;

function TOPPContextMenuEditHelper.OnValidateDeleteText(Sender: TWinControl): Boolean;
var
  PopupComponent: TWinControl;
begin
  result := false;
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if not Assigned(PopupComponent) then
    exit;
  result := (PopupComponent.TextSelectionLength > 0);
end;

function TOPPContextMenuEditHelper.OnValidateOPPBufferOpen(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
end;

function TOPPContextMenuEditHelper.OnValidatePasteText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
  result := result and Clipboard.HasClipboardFormat();
end;

function TOPPContextMenuEditHelper.OnValidateSelectAllText(Sender: TWinControl): Boolean;
var
  PopupComponent: TWinControl;
begin
  result := false;
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if not Assigned(PopupComponent) then
    exit;
  result := PopupComponent.HasTextNonZeroLength;
end;

end.
