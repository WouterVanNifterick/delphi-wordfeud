program WfVCL;

uses
  Vcl.Forms,
  WordFeud.VCL.frmMain in 'WordFeud.VCL.frmMain.pas' {frmMain},
  WordFeud.VCL.frmConnect in 'WordFeud.VCL.frmConnect.pas' {frmConnect};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmConnect, frmConnect);
  Application.Run;
end.
