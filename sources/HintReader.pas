unit HintReader;

interface

uses
  Winapi.Windows,System.SysUtils,
  dxCore, dxPDFDocument, dxPDFViewer, dxPDFCore, dxPDFInteractivity,
  HintBasics;

type
  THintReaderLoadedEvent = procedure(document: TdxPDFDocument) of object;

  TOPPHintReader = class
    destructor Destroy; override;
  private
    fPDFViewer: TdxPDFViewer;
    fOnLoaded: THintReaderLoadedEvent;
    fFileName: String;

    procedure OnPDFLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
    procedure setFileName(value: String);
  public
    constructor create(pdfViewer: TdxPDFViewer);
    property onLoaded: THintReaderLoadedEvent read fOnLoaded write fOnLoaded;
    property fileName: String read fFileName write setFileName;
  end;

implementation

constructor TOPPHintReader.create(pdfViewer: TdxPDFViewer);
begin
  self.fPDFViewer := pdfViewer;
  self.fPDFViewer.Document.onLoaded := self.OnPDFLoaded;
end;

destructor TOPPHintReader.Destroy;
begin
//  self.pdf.Clear;
//  self.pdf.Destroy;
end;

procedure TOPPHintReader.setFileName(value: string);
begin
  fFileName := value;
  self.fPDFViewer.LoadFromFile(fileName);
end;

procedure TOPPHintReader.OnPDFLoaded(Sender: TdxPDFDocument; const AInfo: TdxPDFDocumentLoadInfo);
var
  debugInfo: String;
  i, j: integer;
  link: TdxPDFHyperlink;
  s: String;
begin

  if not sender.AllowContentExtraction then
  begin
    exit;
  end;

  for i:= 0 to sender.PageCount-1 do begin
    for link in sender.PageInfo[i].Hyperlinks do begin
      if link.hint <> '' then begin
      OutputDebugString(link.hint.toWideChar);
      end;
    end;
  end;





  debugInfo := Sender.Information.producer;
  OutputDebugString(debugInfo.toWideChar);

  if assigned(fOnLoaded) then
  begin
    fOnLoaded(Sender);
  end;
end;

end.
