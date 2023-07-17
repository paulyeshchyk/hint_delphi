unit Proxy_OPPHelpPredicate;

interface

uses
  Classes,
  OPP_Help_API;

type

  TProxy_OPPHelpPredicate = class(TInterfacedObject, IOPPHelpPredicate)
  private
    fSubject: IOPPHelpPredicate;
  public
    destructor Destroy; override;

    procedure SetValue(AValue: String);
    function GetValue: String;

    function GetKeywordType: Integer;
    procedure SetKeywordType(const Value: Integer);

    procedure SetFileName(const Value: String);
    function GetFileName: String;

    function GetIsRunnable: Boolean;
    function ReadFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
    function WriteToStream(AStream: TStream): Boolean;
  end;

implementation

uses
  OPP.Help.Predicate;

{ TProxy_OPPHelpPredicate }

destructor TProxy_OPPHelpPredicate.Destroy;
begin
  fSubject := nil;
  inherited;
end;

function TProxy_OPPHelpPredicate.GetFileName: String;
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  result := fSubject.GetFileName;
end;

function TProxy_OPPHelpPredicate.GetIsRunnable: Boolean;
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  result := fSubject.GetIsRunnable;
end;

function TProxy_OPPHelpPredicate.GetKeywordType: Integer;
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  result := fSubject.GetKeywordType;
end;

function TProxy_OPPHelpPredicate.GetValue: String;
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  result := fSubject.GetValue;
end;

function TProxy_OPPHelpPredicate.ReadFromStream(AStream: TStream; moveCursorToStart: Boolean): Boolean;
begin
  result := false;
  if not Assigned(fSubject) then
    exit;
  result := fSubject.ReadFromStream(AStream, moveCursorToStart);
end;

procedure TProxy_OPPHelpPredicate.SetFileName(const Value: String);
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  fSubject.SetFileName(Value);
end;

procedure TProxy_OPPHelpPredicate.SetKeywordType(const Value: Integer);
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  fSubject.SetKeywordType(Value);
end;

procedure TProxy_OPPHelpPredicate.SetValue(AValue: String);
begin
  if fSubject = nil then
    fSubject := TOPPHelpPredicate.Create;
  fSubject.SetValue(AValue);
end;

function TProxy_OPPHelpPredicate.WriteToStream(AStream: TStream): Boolean;
begin
  result := false;
  if not Assigned(fSubject) then
    exit;
  result := fSubject.WriteToStream(AStream);
end;

end.
