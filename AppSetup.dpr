program AppSetup;

{$R 'AppDemo.res' 'AppDemo.rc'}

uses
  Forms,
  FormAppSetup in 'FormAppSetup.pas' {FrmAppSetup},
  AppConsts in 'AppConsts.pas',
  SysCaller in 'SysCaller.pas',
  WinAppCore in 'WinAppCore.pas',
  FormMsg in 'FormMsg.pas' {FrmMsg},
  FormDMUnzip in 'FormDMUnzip.pas' {DMUnzip: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AppSetup';
  Application.CreateForm(TDMUnzip, DMUnzip);
  Application.CreateForm(TFrmAppSetup, FrmAppSetup);
  Application.Run;
end.
