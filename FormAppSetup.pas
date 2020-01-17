{**************************************************************************************************
 COMPLETE DELPHI APP INSTALLER DEMO PROJECT: ( 32 Bits ). V1.0. January 2020.
***************************************************************************************************

 This project covers most of the requisites a modern setup application needs to achieve on Windows,
 without the help of any scripting system. It is entirely made from the scratch with Delphi 7, thus
 providing you all the freedom you may need.

 IMPORTANT: This project contains compiled exe files as Zipped resources. Please read "readme.txt"
 file for a better understanding about the compiling mechanism.

***************************************************************************************************
 DISCLAIMER: All projects and units are provided "AS IS" with no warranty of any kind.
***************************************************************************************************
* Thanks and enjoy,
* Kim Herrero / kim@kimherrero.com
***************************************************************************************************}
unit FormAppSetup;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ToolEdit, ExtCtrls, Buttons;

type
  TFrmAppSetup = class(TForm)
    lbIns: TLabel;
    lbDat: TLabel;
    Label1: TLabel;
    BtClose: TButton;
    InsFolder: TEdit;
    DatFolder: TEdit;
    btSelIns: TButton;
    btSelDat: TButton;
    BtInstall: TBitBtn;
    Label3: TLabel;
    cbUser: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btSelInsClick(Sender: TObject);
    procedure BtCloseClick(Sender: TObject);
    procedure cbUserChange(Sender: TObject);
    procedure BtInstallClick(Sender: TObject);
  private
    { Private declarations }
    function  TestFolders( Delete: Boolean ): Boolean;
    procedure AppUpdate;
  public
    { Public declarations }
  end;

var
  FrmAppSetup: TFrmAppSetup;

implementation

uses WinAppCore, IniFiles, SysCaller, AppConsts, SysUtils, FormDMUnzip, FormMsg;

{$R *.dfm}

procedure TFrmAppSetup.FormCreate(Sender: TObject);
const     APP_OK = 'Application seems to be installed, do you want to reinstall?';
          APP_KO = 'Installation corrupted, do you want to reinstall?';
          APP_REPAIR = 'About to reinstall Application, proceed?';

var       Reinstall: Boolean;
begin
  Reinstall := False;
  Hide;

  // installation found
  If LoadPaths then begin

     If DMUnzip.AppSendClose then begin

        Param := UpperCase( GetParamInfo( ParamInfo ) );
        If Not ( Param = '/INSTALL' ) then begin

           If Not ( Param = '/UPDATE' ) then begin

               // ask for optional installation. Of course more files can be checked
               If FileExists( InstallDir + SC_APP_EXE ) then Reinstall := ShowMsg( APP_OK, True )
               // ask to repair installation
               else Reinstall := ShowMsg( APP_KO, True );

           // forced application update by parameter /UPDATE
           end else AppUpdate;

        // forced repair by parameter /INSTALL
        end else if ShowMsg( APP_REPAIR, True ) then BtInstallClick( nil );

     end else ShowMsg( MSG_INMEM );

  end else Reinstall := True;  // no previous installation

  // make form visible
  If Reinstall then begin

     cbUserChange( nil );
     Visible := True;

  end else Application.Terminate;

end;

procedure TFrmAppSetup.cbUserChange(Sender: TObject);
begin
  InstallDir := GetSystemFolderName( sfPrograms ) + SC_APP_FOLDER;

  If cbUser.ItemIndex = 0 then begin  // All users

     UsrDataDir   := GetSystemFolderName( sfAllData ) + SC_APP_FOLDER;
     MenuDir      := GetSystemFolderName( sfAllStartMenu ) + SC_APP_FOLDER;
     DesktopDir   := GetSystemFolderName( sfAllDesktop ) + '\';

  end else begin  // Current user

     UsrDataDir   := GetSystemFolderName( sfUsrData ) + SC_APP_FOLDER;
     MenuDir      := GetSystemFolderName( sfUsrStartMenu ) + SC_APP_FOLDER;
     DesktopDir   := GetSystemFolderName( sfUsrDesktop ) + '\';

  end;

  InsFolder.Text := InstallDir;
  DatFolder.Text := UsrDataDir;

  BtInstall.Enabled := TestFolders( True );

end;

function TFrmAppSetup.TestFolders(Delete: Boolean): Boolean;
const    TST = 'test.tmp';
var      H1, H2: Integer; ok1, ok2: Boolean;
begin
  // Test access to Installation folder
  If IsAdministrator then begin

    ForceDirectories( InsFolder.Text );
    H1 := FileCreate( InsFolder.Text + TST);
    FileClose( H1 );
    DeleteFile( InsFolder.Text + TST);

    Ok1 := ( H1 > -1 );
    If Ok1 Then InsFolder.Color :=  $00DFFFD5
    else InsFolder.Color :=  $00CCCCFF;

  end else Ok1 := True;

  // Test access to Data folder
  ForceDirectories( DatFolder.Text );
  H2 := FileCreate( DatFolder.Text + TST);
  FileClose( H2 );
  DeleteFile( DatFolder.Text + TST);

  Ok2 := ( H2 > -1 );
  If Ok2 Then DatFolder.Color :=  $00DFFFD5
  else DatFolder.Color :=  $00CCCCFF;

  // remove test files & folders if empty
  If Delete and DirectoryIsEmpty( DatFolder.Text ) then
     RemoveDir( DatFolder.Text );

  If Delete and DirectoryIsEmpty( InsFolder.Text ) then
     RemoveDir( InsFolder.Text );

  Result := Ok1 and Ok2;
end;

procedure TFrmAppSetup.btSelInsClick(Sender: TObject);
var       S: String;
begin
  TestFolders( False );

  Case TControl( Sender ).Tag Of
     1 : S := BrowseDialog( 'Select installation folder', InsFolder.Text );
     2 : S := BrowseDialog( 'Select data folder', DatFolder.Text );
  end;

  If ( S <> '' ) then Case TControl( Sender ).Tag Of
     1 : InsFolder.Text := S;
     2 : DatFolder.Text := S;
  end;

  BtInstall.Enabled := TestFolders( True );
end;

procedure TFrmAppSetup.BtInstallClick(Sender: TObject);
const     MSG_LAUNCH = 'Application installed, launch?';
          MSG_FAIL   = 'Error: Setup failed';

var       I: TIniFile; TempFld, TempZip, AFile, UpdExe: String; Ok: Boolean;
begin
  // create a temp folder
  TempFld := NewTempFolder( UsrTempDir, 'AppInstall' );
  AFile := TempFld + SC_ZIP_FILE;
  TempZip := ChangeFileExt( AFile, '' );
  ForceDirectories( TempZip );

  DMUnzip.UnZipper.TempDirectory := UsrTempDir;
  Ok := False;

  // extract compressed setup file
  ExtractResource( 'ZIP', AFile );
  DMUnzip.UnzipFile( AFile, TempZip, '*.*' );

  AFile := DMUnzip.UnZipper.BaseDirectory + '\bin.zip';
  If FileExists ( AFile ) then begin

     // extract appupdater.exe
     DMUnzip.UnzipFile( AFile, TempFld, SC_UPD_EXE );
     UpdExe := TempFld + SC_UPD_EXE;

     If FileExists ( UpdExe ) then begin

        // Ini setup configuration
        AFile := TempFld + SC_INI_SETUP;
        I := TIniFile.Create( AFile );

        I.WriteString( SC_INI_GROUP, REG_APP_FLD, InstallDir );
        I.WriteString( SC_INI_GROUP, REG_USR_FLD, UsrDataDir );
        I.WriteString( SC_INI_GROUP, REG_DSK_ICO, DesktopDir );
        I.WriteString( SC_INI_GROUP, REG_MNU_FLD, MenuDir );

        I.Free;

        // launch appupdater.exe with specified configuration
        TempZip := SafeParam( Format( '/INSTALL:%s', [ TempFld ] ) ) + ' ' + SafeParam( Application.ExeName );
        If ShellNewProcess( UpdExe, TempZip, True, True ) then begin

           AFile := InstallDir + SC_APP_EXE;
           If FileExists( AFile ) and FileExists( TempFld + SC_INSTALL_OK ) then begin

              Repaint;

              If ShowMsg( MSG_LAUNCH, True ) then begin
                 ShellNewProcess( AFile, '', True, False );
                 SetForegroundWindowClass( SC_APP_CLASS );
                 Hide;
              end;

              Ok := True;
              Close;

           end;

        end;

        // delete temp folder
        ShellDeleteFolder( TempFld );

    end;

  end;

  If Not Ok then
     ShowMsg( MSG_FAIL );

end;

procedure TFrmAppSetup.AppUpdate;
const     MSG_ERR = 'Error updating application';
var       TempFld, ZipFile, ExeFile, AFlag : String; ok : Boolean;
begin
  // ParamInfo is the zip file, copy to a new temp folder
  If FileExists( ParamInfo ) then begin
     TempFld := NewTempFolder( UsrTempDir, 'AppUpdate' );
     ZipFile := TempFld + ExtractFileName( ParamInfo );
     ForceDirectories( TempFld );
     CopyFile( PChar( ParamInfo ), PChar( ZipFile ), False );
  end;

  ExeFile := InstallDir + SC_UPD_EXE;
  ok := False;

  // Call updater and wait to finish
  If FileExists( ZipFile ) and FileExists( ExeFile ) and
     ShellNewProcess( ExeFile, SafeParam( '/UPDATE:' + ZipFile ), True, True ) then begin

     ExeFile := InstallDir + SC_APP_EXE;
     AFlag   := TempFld + SC_INSTALL_OK;

     // If both App and ok flag exist then launch app
     If FileExists( ExeFile ) and FileExists( AFlag ) then begin
        ShellNewProcess( ExeFile, '', True, False );
        SetForegroundWindowClass( SC_APP_CLASS );
        Ok := True;
     end;

     // remove temp dir
     ShellDeleteFolder( TempFld );

  end;

  If Not Ok then
     ShowMsg( MSG_ERR );

  Close;

end;

procedure TFrmAppSetup.BtCloseClick(Sender: TObject);
begin
  Close;
end;



end.
