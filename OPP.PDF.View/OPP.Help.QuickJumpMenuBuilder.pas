unit OPP.Help.QuickJumpMenuBuilder;

interface

uses
  System.Classes, Vcl.ActnList,
  dxBar,
  OPP.Help.Map;

type
  TOPPHelpJumpActionCompletion = reference to procedure(AHelpMap: TOPPHelpMap);

  TOPPHelpJumpAction = class(TAction)
  private
    fHelpMap: TOPPHelpMap;
    fInvocation: TOPPHelpJumpActionCompletion;
    procedure onCustomExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AHelpMap: TOPPHelpMap; invocation: TOPPHelpJumpActionCompletion); overload;
  end;

  TOPPHelpQuickJumpMenuBuilderCompletion = reference to procedure(AAction: TAction);
  TOPPHelpLoadResourceCompletion = reference to procedure(AStream: TCustomMemoryStream);

  TOPPHelpQuickJumpMenuBuilder = class
    class procedure Build(AOwner: TComponent; completion: TOPPHelpQuickJumpMenuBuilderCompletion; invocation: TOPPHelpJumpActionCompletion);
    class procedure LoadResource(AResourceName: PChar; completion: TOPPHelpLoadResourceCompletion);
  end;

implementation

uses
  System.SysUtils,
  WinAPI.Windows, System.Generics.Collections,
  OPP.Help.Log,
  OPP.Help.Map.Parser.JSON;

const
  kContext = 'TOPPHelpQuickJumpMenuBuilder';

{ TOPPHelpQuickJumpMenuBuilder }

class procedure TOPPHelpQuickJumpMenuBuilder.Build(AOwner: TComponent; completion: TOPPHelpQuickJumpMenuBuilderCompletion; invocation: TOPPHelpJumpActionCompletion);
begin
  TOPPHelpQuickJumpMenuBuilder.LoadResource(PChar('RC_HELP_DROPDOWN_MENU'),
    procedure(AStream: TCustomMemoryStream)
    var
      fBarLink: TdxBarItemLink;
      fAction: TOPPHelpJumpAction;
    begin
      if not Assigned(AStream) then
      begin
        eventLogger.Warning('Stream is not defined', kContext);
        exit;
      end;

      TOPPHelpMapRESTParser.readStream(AStream,
        procedure(ASet: TOPPHelpMapSet; Error: Exception)
        var
          fItem: TOPPHelpMap;
        begin
          if ASet.list.Count = 0 then
            exit;
          for fItem in ASet.list do
          begin
            fAction := TOPPHelpJumpAction.Create(AOwner, fItem, invocation);
            completion(fAction);
          end;
        end);
    end);
end;

class procedure TOPPHelpQuickJumpMenuBuilder.LoadResource(AResourceName: PChar; completion: TOPPHelpLoadResourceCompletion);
var
  stream: TResourceStream;
begin
  if (FindResource(hInstance, AResourceName, RT_RCDATA) = 0) then
  begin
    eventLogger.Error(Format('Resource ''%s'' not found', [AResourceName]), kContext);
    if Assigned(completion) then
      completion(nil);
    exit;
  end;

  stream := TResourceStream.Create(hInstance, AResourceName, RT_RCDATA);
  try
    if Assigned(completion) then
      completion(stream);
  finally
    stream.Free;
  end;

end;

{ TOPPHelpJumpAction }

constructor TOPPHelpJumpAction.Create(AOwner: TComponent; AHelpMap: TOPPHelpMap; invocation: TOPPHelpJumpActionCompletion);
begin
  inherited Create(AOwner);
  fHelpMap := AHelpMap;
  self.OnExecute := onCustomExecute;
  self.fInvocation := invocation;
  if Assigned(fHelpMap) then
  begin
    caption := fHelpMap.ComponentIdentifier;
  end;
end;

procedure TOPPHelpJumpAction.onCustomExecute(Sender: TObject);
begin
  if Assigned(fInvocation) then
    fInvocation(fHelpMap);
end;

end.
