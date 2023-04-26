unit OPP.ContextMenu.Edit;

interface

uses Vcl.Menus, System.Classes, WinAPI.Messages, WinAPI.Windows, Vcl.Controls;

type
  TOPPContextMenuItemOnValidate = reference to function(Sender: TWinControl): Boolean;

  TOPPContextMenuItem = class(TMenuItem)
  private
    fOnValidate: TOPPContextMenuItemOnValidate;
  public
    property OnValidate: TOPPContextMenuItemOnValidate read fOnValidate write fOnValidate;
  end;

  TOPPContextMenuEdit = class(TPopupMenu)
  protected
    procedure Popup(X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function CreateCustomMenuItem(AOwner: TComponent; Caption: String; onClick: TNotifyEvent; OnValidate: TOPPContextMenuItemOnValidate): TOPPContextMenuItem;
  end;

  TOPPContextMenuEditHelper = class Helper for TOPPContextMenuEdit
  private
    class function doCheckIfSenderIsEditable(Sender: TObject): Boolean;
  public
    class function GetPopupComponentAsEditControl(Sender: TObject): TComponent;

    class procedure OnCutText(Sender: TObject);
    class procedure OnCopyText(Sender: TObject);
    class procedure OnPasteText(Sender: TObject);
    class procedure OnDeleteText(Sender: TObject);
    class procedure OnSelectAllText(Sender: TObject);
    class procedure OnOPPBufferOpen(Sender: TObject);

    class function OnValidateCutText(Sender: TWinControl): Boolean;
    class function OnValidateCopyText(Sender: TWinControl): Boolean;
    class function OnValidatePasteText(Sender: TWinControl): Boolean;
    class function OnValidateDeleteText(Sender: TWinControl): Boolean;
    class function OnValidateSelectAllText(Sender: TWinControl): Boolean;
    class function OnValidateOPPBufferOpen(Sender: TWinControl): Boolean;

  end;

implementation

uses OPP.Help.System.Control,
  Vcl.ClipBrd,
  System.RTTI,
  OPP.Buffer.Clipboard, cxEdit;

{ TOPPContextMenuEdit }

constructor TOPPContextMenuEdit.Create(AOwner: TComponent);
var
  fItem: TMenuItem;
begin
  inherited Create(AOwner);

  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Вырезать', TOPPContextMenuEdit.OnCutText, TOPPContextMenuEdit.OnValidateCutText));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Копировать', TOPPContextMenuEdit.OnCopyText, TOPPContextMenuEdit.OnValidateCutText));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Вставить', TOPPContextMenuEdit.OnPasteText, TOPPContextMenuEdit.OnValidatePasteText));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Удалить', TOPPContextMenuEdit.OnDeleteText, TOPPContextMenuEdit.OnValidateCutText));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, '-', nil, nil));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Выделить всё', TOPPContextMenuEdit.OnSelectAllText, TOPPContextMenuEdit.OnValidateSelectAllText));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, '-', nil, nil));
  self.Items.Add(TOPPContextMenuEdit.CreateCustomMenuItem(self, 'Буфер обмена ГС', TOPPContextMenuEdit.OnOPPBufferOpen, TOPPContextMenuEdit.OnValidateCutText));
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
  isEnabledItem: Boolean;
  fWinControl: TWinControl;
begin
  for item in self.Items do
  begin
    if item is TOPPContextMenuItem then
    begin
      if Assigned(TOPPContextMenuItem(item).OnValidate) and (self.PopupComponent is TWinControl) then
      begin
        item.Enabled := TOPPContextMenuItem(item).OnValidate(TWinControl(self.PopupComponent));
      end else begin
        item.Enabled := false;
      end;
    end;
  end;

  inherited;
end;

{ TOPPContextMenuEditHelper }

class function TOPPContextMenuEditHelper.doCheckIfSenderIsEditable(Sender: TObject): Boolean;
var
  PopupComponent: TComponent;
begin
  result := false;
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if Assigned(PopupComponent) then
    result := true;
end;

class function TOPPContextMenuEditHelper.GetPopupComponentAsEditControl(Sender: TObject): TComponent;
var
  PopupComponent: TComponent;
  fValue: TValue;
begin
  result := nil;
  if not(Sender is TOPPContextMenuItem) then
    exit;
  if not(TOPPContextMenuItem(Sender).Owner is TOPPContextMenuEdit) then
    exit;
  PopupComponent := TOPPContextMenuEdit(TOPPContextMenuItem(Sender).Owner).PopupComponent;
  if not Assigned(PopupComponent) then
  exit;

  if PopupComponent is TcxCustomEdit then begin
    result := TcxCustomEdit(PopupComponent).InnerControl;
  end else begin
    result := PopupComponent;
  end

end;

class procedure TOPPContextMenuEditHelper.OnCopyText(Sender: TObject);
begin
  if not doCheckIfSenderIsEditable(Sender) then
    exit;
end;

class procedure TOPPContextMenuEditHelper.OnCutText(Sender: TObject);
begin
  if not doCheckIfSenderIsEditable(Sender) then
    exit;
end;

class procedure TOPPContextMenuEditHelper.OnDeleteText(Sender: TObject);
begin
  if not doCheckIfSenderIsEditable(Sender) then
    exit;
end;

class procedure TOPPContextMenuEditHelper.OnOPPBufferOpen(Sender: TObject);
begin
  if not doCheckIfSenderIsEditable(Sender) then
    exit;
end;

class procedure TOPPContextMenuEditHelper.OnPasteText(Sender: TObject);
begin
  if not doCheckIfSenderIsEditable(Sender) then
    exit;
end;

class procedure TOPPContextMenuEditHelper.OnSelectAllText(Sender: TObject);
var
  PopupComponent : TComponent;
begin
  PopupComponent := GetPopupComponentAsEditControl(Sender);
  if assigned(PopupComponent) and (PopupComponent is TWinControl) then
  SendMessage(TWinControl(PopupComponent).Handle, EM_SETSEL, 0, -1);
end;

class function TOPPContextMenuEditHelper.OnValidateCopyText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
end;

class function TOPPContextMenuEditHelper.OnValidateCutText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
end;

class function TOPPContextMenuEditHelper.OnValidateDeleteText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
end;

class function TOPPContextMenuEditHelper.OnValidateOPPBufferOpen(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
end;

class function TOPPContextMenuEditHelper.OnValidatePasteText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextProp;
  result := result and Clipboard.HasClipboardFormat();
end;

class function TOPPContextMenuEditHelper.OnValidateSelectAllText(Sender: TWinControl): Boolean;
begin
  result := false;
  if not Assigned(Sender) then
    exit;
  result := Sender.HasTextNonZeroLength;
end;

end.
