unit devEx;

interface

type
  TdxPDFDocument = class;
  TdxPDFDocumentLoadInfo = class;
  TdxPDFDocumentInformation = class;

  TdxPDFImageList = class

  end;

  TdxPointF = record

  end;

  TdxPDFPageInfo = record
    Images: TdxPDFImageList;
    Text: string;
    Size: TdxPointF;
    UserUnit: Integer;
  end;

  TdxPDFDocumentLoadedEvent = procedure(Sender: TdxPDFDocument;
    const AInfo: TdxPDFDocumentLoadInfo) of object;

  TdxPDFDocument = class
  private
    fOnLoaded: TdxPDFDocumentLoadedEvent;
    fInformation: TdxPDFDocumentInformation;
  public
    property onLoaded: TdxPDFDocumentLoadedEvent read fOnLoaded write fOnLoaded;
    property Information: TdxPDFDocumentInformation read fInformation
      write fInformation;
    procedure Clear;
    procedure LoadFromFile(const fileName: String);
  end;

  TdxPDFDocumentInformation = class
  private
    fProducer: String;
  public
    property producer: String read fProducer write fProducer;
  end;

  TdxPDFDocumentLoadInfo = class

  end;

implementation

procedure TdxPDFDocument.Clear;
begin

end;

procedure TdxPDFDocument.LoadFromFile(const fileName: String);
begin

end;

end.
