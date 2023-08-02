program TestProject1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TestUnit2,
  lazviewstackbookmarks in '..\lazviewstackbookmarks.pas';

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TLazViewStackBookmarks, frmViewStackBM);
  frmViewStackBM.Left:=Form1.Left+Form1.Width+15;
  frmViewStackBM.Show;
  Form1.btnChangeFilenameClick(Form1);
  Application.Run;
end.

