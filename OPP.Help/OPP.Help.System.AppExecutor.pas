unit OPP.Help.System.AppExecutor;

interface

uses
  System.Generics.Collections,
  System.TypInfo,
  Winapi.Windows,
  Vcl.Forms,

  OPP.Help.System.Messaging;

type
  TOPPHelpSystemAppExecutionResultType = (rtFailedDueUnableToRunProcess, rtNewInstance, rtExistingInstance);

  TOPPHelpSystemAppExecutorCompletion = reference to procedure(AList: TList<THandle>; executionResult: TOPPHelpSystemAppExecutionResultType);

  TOPPHelpSystemAppExecutor = class
  public
    class function FindAnyClass(const Name: string): Pointer;
    class function FindClass(AName: String): Pointer;
    class procedure Execute(Appname: String; completion: TOPPHelpSystemAppExecutorCompletion; AActivationDelay: Cardinal = 1000);
  end;

implementation

uses
  System.Rtti,
  System.StrUtils,
  System.SysUtils,
  System.Classes;

const
  kApplicationFormClassName = 'TOPPHelpPreviewForm';

  { TOPPHelpSystemAppExecutor }

class function TOPPHelpSystemAppExecutor.FindClass(AName: String): Pointer;
var
  fFoundClass: TClass;
  fRTTIContext: TRttiContext;
  fFoundType: TRttiType;
begin
  result := nil;
  fRTTIContext := TRttiContext.Create;
  try
    fFoundType := fRTTIContext.FindType(AName);
    if (fFoundType <> nil) and (fFoundType.IsInstance) then
    begin
      fFoundClass := fFoundType.AsInstance.MetaClassType;
      result := fFoundClass.ClassInfo;
    end;
  finally
    fRTTIContext.Free;
  end;
end;

class function TOPPHelpSystemAppExecutor.FindAnyClass(const Name: string): Pointer;
var
  ctx: TRttiContext;
  fType: TRttiType;
  fTypeArray: TArray<TRttiType>;
begin
  result := nil;
  ctx := TRttiContext.Create;
  fTypeArray := ctx.GetTypes;
  for fType in fTypeArray do
  begin
    if fType.IsInstance and (EndsText(Name, fType.Name)) then
    begin
      result := fType.AsInstance.MetaClassType.ClassInfo;
      break;
    end;
  end;
  ctx.Free;
end;

class procedure TOPPHelpSystemAppExecutor.Execute(Appname: String; completion: TOPPHelpSystemAppExecutorCompletion; AActivationDelay: Cardinal);
var
  fWindowClassHandleList: TList<THandle>;
  fSelfHandle: THandle;
  fOPPViewerClassName: String;
begin

  fOPPViewerClassName := kApplicationFormClassName; // GetTypeData(AViewerClassInfo).ClassType.ClassName;

  fSelfHandle := Application.Handle;

  fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
  if Assigned(fWindowClassHandleList) and (fWindowClassHandleList.Count <> 0) then
  begin
    completion(fWindowClassHandleList, rtExistingInstance);
    exit;
  end;

  TOPPSystemMessageHelper.RunProcess(Appname, fSelfHandle, AActivationDelay,
    procedure(ARunResultType: Exception)
    begin
      if ARunResultType = nil then
      begin
        fWindowClassHandleList := TOPPSystemMessageHelper.GetWindowClassHandleList(fOPPViewerClassName);
        completion(fWindowClassHandleList, rtNewInstance);
        exit;
      end;

      completion(nil, rtFailedDueUnableToRunProcess);

    end);

end;

end.
