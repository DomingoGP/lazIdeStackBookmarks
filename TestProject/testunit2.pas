unit TestUnit2;

{
  WARNING: Do a clean rebuild.


}


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnChangeFilename:TButton;
    Button1:TButton;
    edBaseDir:TEdit;
    edFileName:TEdit;
    edSourceCode:TSynEdit;
    edTest:TSynEdit;
    Memo1:TMemo;
    procedure btnChangeFilenameClick(Sender:TObject);
    procedure Button1Click(Sender:TObject);
    procedure FormClose(Sender:TObject; var CloseAction:TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
  uses
    LazViewStackBookmarks;

{ TForm1 }

var
  gNumber:integer=0;

procedure TForm1.btnChangeFilenameClick(Sender:TObject);
begin
  Inc(gNumber);
  DoChangeActiveEditor(edSourceCode,'c:\StockBookmarks.Unit'+IntToStr(gNumber)+'.pas','Unit'+IntToStr(gNumber));
end;

procedure TForm1.Button1Click(Sender:TObject);
var
  wIsRelative:boolean;
begin
  Memo1.Text:=GetNormalizedFileName(edBaseDir.Text,edFileName.Text,wIsRelative);
end;

procedure TForm1.FormClose(Sender:TObject; var CloseAction:TCloseAction);
begin
  DoChangeActiveEditor(nil,'');
  FreeBookmarks;
  frmViewStackBM:=nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EnableCaptureIdeEvents(True);
end;

end.

