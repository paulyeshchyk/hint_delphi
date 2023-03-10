unit OPPHintMain;

interface

uses
  vcl.forms, HintDialog, HintBasics;

  procedure showHintDialog(userInfo: TOPPHintUserInfo); stdcall;

  exports showHintDialog;

implementation

  procedure closeHintDialog(form: TForm);
  begin
    form.Close;
  end;

  procedure showHintDialog(userInfo: TOPPHintUserInfo);
  var
    sampleForm: TOPPHintDialog;
  begin
    sampleForm := TOPPHintDialog.Create(nil);
    sampleForm.userInfo := userInfo;
    sampleForm.dialogCompletion := closeHintDialog;
    sampleForm.ShowModal;
  end;

end.
