program Wordfeud;

uses
  FMX.Forms,
  WordFeud.frmMain in 'WordFeud.frmMain.pas' {Form9},
  WvN.WordFeud in '..\..\WvN.WordFeud.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
