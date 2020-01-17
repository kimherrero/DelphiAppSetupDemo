program AppUpdater;

{$R 'AppUpdaterRes.res' 'AppUpdaterRes.rc'}

uses
  Forms,
  FormDMUpdater in 'FormDMUpdater.pas' {DMUpdater: TDataModule},
  WinAppCore in 'WinAppCore.pas',
  FormMsg in 'FormMsg.pas' {FrmMsg},
  AppConsts in 'AppConsts.pas',
  FormProgress in 'FormProgress.pas' {FrmProgress},
  FormDMUnzip in 'FormDMUnzip.pas' {DMUnzip: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AppUpdater';
  Application.CreateForm(TDMUpdater, DMUpdater);
  Application.Run;
end.
