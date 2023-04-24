unit OPP.Buffer.Manager.Settings.Data;

interface

uses
  System.Types,
  OPP.Help.System.Codable,
  OPP.Help.System.Codable.Helper,
  System.Generics.Collections;

type

  TOPPBufferManagerSettingsColumnSort = record
    FieldName: String;
    SortIndex: Integer;
    SortType: Integer;
  end;

  TOPPBufferManagerSettingsData = class(TOPPCodable)
  private
    fCurrentFileName: String;
    fShortcut: Word;
    fRecordsCountLimit: Integer;
    fIsExternalAllowed: Boolean;
    fCanSaveFormFrame: Boolean;
    fUseRecordsCountLimit: Boolean;
    fColumnSort: TList<TOPPBufferManagerSettingsColumnSort>;
    fFormFrame: TRect;

  public
    class procedure Save(AFileName: String; AData: TOPPBufferManagerSettingsData);
    class procedure Load(AFileName: String; out AData: TOPPBufferManagerSettingsData);

    constructor Create;override;
    destructor Destroy;override;

    procedure SetColumnSortArray(AArray:TArray<TOPPBufferManagerSettingsColumnSort>);
    property CurrentFileName: String read fCurrentFileName write fCurrentFileName;
    property Shortcut: Word read fShortcut write fShortcut;
    property RecordsCountLimit: Integer read fRecordsCountLimit write fRecordsCountLimit;
    property IsExternalAllowed: Boolean read fIsExternalAllowed write fIsExternalAllowed;
    property CanSaveFormFrame: Boolean read fCanSaveFormFrame write fCanSaveFormFrame;
    property UseRecordsCountLimit: Boolean read fUseRecordsCountLimit write fUseRecordsCountLimit;
    property ColumnSort: TList<TOPPBufferManagerSettingsColumnSort> read fColumnSort write fColumnSort;
    property FormFrame: TRect read fFormFrame write fFormFrame;
  end;

implementation

uses
  System.SysUtils,
  OPP.Help.Log;

const
  SOPPBufferManagerSettingsFileName = 'OPPBufferManager.settings';

{ TOPPBufferManagerSettingsData }

constructor TOPPBufferManagerSettingsData.Create;
begin
  inherited Create;
  self.Shortcut := 0;
  self.RecordsCountLimit := 20;
  self.IsExternalAllowed := false;
  self.CanSaveFormFrame := true;
  self.UseRecordsCountLimit := true;
  self.ColumnSort := TList<TOPPBufferManagerSettingsColumnSort>.Create;
end;

destructor TOPPBufferManagerSettingsData.Destroy;
begin
  fColumnSort.Free;
  inherited;
end;

class procedure TOPPBufferManagerSettingsData.Load(AFileName: String; out AData: TOPPBufferManagerSettingsData);
begin
  try
    TOPPCodableHelper<TOPPBufferManagerSettingsData>.Decode(SOPPBufferManagerSettingsFileName, AData);
  except
    on E: Exception do
    begin
      AData := nil;
      eventLogger.Error(E);
    end;
  end;
end;

class procedure TOPPBufferManagerSettingsData.Save(AFileName: String; AData: TOPPBufferManagerSettingsData);
begin
  try
    TOPPCodableHelper<TOPPBufferManagerSettingsData>.Encode(AFileName, AData);
  except
    on E: Exception do
    begin
      eventLogger.Error(E);
    end;
  end;
end;

procedure TOPPBufferManagerSettingsData.SetColumnSortArray(AArray: TArray<TOPPBufferManagerSettingsColumnSort>);
begin
  self.ColumnSort.Clear;
  self.ColumnSort.AddRange(AArray);
end;

end.
