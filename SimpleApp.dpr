program SimpleApp;

uses
  Forms,
  FormSimpleApp in 'FormSimpleApp.pas' {FrmSimpleApp},
  WinAppCore in 'WinAppCore.pas',
  AppConsts in 'AppConsts.pas',
  SysCaller in 'SysCaller.pas',
  FormMsg in 'FormMsg.pas' {FrmMsg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple App';
  Application.CreateForm(TFrmSimpleApp, FrmSimpleApp);
  Application.Run;
end.
