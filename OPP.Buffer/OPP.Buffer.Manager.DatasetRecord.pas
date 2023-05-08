unit OPP.Buffer.Manager.DatasetRecord;

interface

uses
  OPP.Buffer.OPPInfo;

type
  TOPPBufferManagerRecord = class
  private
    fOPPInfo: TOPPBufferOPPInfo;
    fText: Variant;
    fIsFixed: Boolean;
    fSortIndex: Integer;
  public
    property OPPInfo: TOPPBufferOPPInfo read fOPPInfo write fOPPInfo;
    property Text: Variant read fText write fText;
    property IsFixed: Boolean read fIsFixed write fIsFixed;
    property SortIndex: Integer read fSortIndex write fSortIndex;
  end;

implementation

end.
