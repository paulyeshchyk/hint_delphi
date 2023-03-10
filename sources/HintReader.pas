unit HintReader;

interface

  uses
    Winapi.Windows,
    dxPDFDocument;

  type
    THintReaderLoadedEvent = procedure(document: TdxPDFDocument) of object;

    TOPPHintReader = class
      destructor Destroy; override;
    private
      pdf: TdxPdfDocument;
      fOnLoaded: THintReaderLoadedEvent;
      fFileName: String;

      procedure OnPDFLoaded(Sender: TdxPdfDocument; const AInfo: TdxPDFDocumentLoadInfo);
      procedure setFileName(value: String);
    public
      constructor create();
      property OnLoaded: THintReaderLoadedEvent read fOnLoaded write fOnLoaded;
      property fileName: String read fFileName write setFileName;
    end;

implementation
uses HintBasics;

  constructor TOPPHintReader.create();
  begin
    self.pdf := TdxPdfDocument.create;
    self.pdf.OnLoaded := self.OnPDFLoaded;
  end;

  destructor TOPPHintReader.Destroy;
  begin
    self.pdf.Clear;
    self.pdf.Destroy;
  end;

  procedure TOPPHintReader.setFileName(value: string);
  begin
    fFileName := value;
    self.pdf.LoadFromFile(filename);
  end;

  procedure TOPPHintReader.OnPDFLoaded(Sender: TdxPdfDocument; const AInfo: TdxPDFDocumentLoadInfo);
  var
    debugInfo: String;
  begin

    debugInfo := Sender.Information.producer;
    OutputDebugString(debugInfo.toWideChar);

    if assigned(fOnLoaded) then begin
      fOnLoaded(sender);
    end;
  end;

end.
