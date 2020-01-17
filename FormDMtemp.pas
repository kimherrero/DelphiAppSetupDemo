{*********************************************************************************************
* DELPHI SIMPLE APP SETUP DEMO PROJECT. V1.0. January 2020.
**********************************************************************************************
* This project covers the basics of a setup application made from the scratch with Delphi 7.
* Please, see readme.txt for detailed information.
***********************************************************************************************
* Disclaimer: This project and all units are provided "AS IS" with no warranty of any kind.
**********************************************************************************************
* Thanks and enjoy,
* Kim Herrero / kim@kimherrero.com
**********************************************************************************************}
unit FormDMtemp;

interface

uses
  SysUtils, Classes, FormDMUnzip, AbBase, AbArcTyp, AbBrowse, AbZBrows, AbUnzper;

type
  TDMUpdater = class(TDMUnzip)
    procedure DataModuleCreate(Sender: TObject);
    procedure UnZipperConfirmOverwrite(var Name: String; var Confirm: Boolean);
    procedure UnZipperArchiveItemProgress(Sender: TObject; Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
  private
    { Private declarations }
    procedure AppInstall;
    procedure AppUpdate;
    procedure AppUnInstall;
    procedure AppDelete;
    function  DeleteFiles( const Folder, log: String ): Boolean;
    function  RegisterInstall: Boolean;
    function  ExtractFiles( AZipFile: String ): Boolean;
    function  ExtractFilesLog( AZipFile, Destination: String; KeepLog: Boolean ): Boolean;
    function  CreateShortcuts: Boolean;
  public
    { Public declarations }
  end;

var
  DMUpdater: TDMUpdater;

implementation

uses Forms, SysCaller, WinAppCore, Windows, IniFiles, Registry, FormMsg, AppConsts, FormProgress;

var  Param, ParamInfo: String;
     LogFile: TStringList;


{$R *.dfm}

procedure TDMUpdater.DataModuleCreate(Sender: TObject);
begin
  inherited;
  Param := UpperCase( GetParamInfo( ParamInfo ) );
  LoadPaths;

  If Not ( Param = '/INSTALL' ) then begin

        If (Param = '/UPDATE' ) then AppUpdate
          else If (Param = '/UNINSTALL' ) then AppUnInstall
               else If (Param = '/ZAP' )  // not possible from InstallDir
                    and ( LowerCase( ExtractFileDir( ParamStr( 0 ) ) ) <> LowerCase( InstallDir ) ) then AppDelete;

  end else AppInstall;

  Application.Terminate;
end;

procedure TDMUpdater.UnZipperArchiveItemProgress(Sender: TObject; Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
var       S: String;
begin
  S := LowerCase( Item.FileName );
  // avoid duplicate file log
  If Assigned( LogFile ) and ( LogFile.IndexOf( S ) < 0 ) then
     LogFile.Add( S );
  FrmProgress.Repaint;
end;

procedure TDMUpdater.UnZipperConfirmOverwrite(var Name: String; var Confirm: Boolean);
begin
  Confirm := True;
end;

procedure TDMUpdater.AppInstall;
const     MSG_ERR = 'Installer found an error.';
var       I: TIniFile; Path, AFile, CallerApp: String; DWIM, DWIL, DWRM, DWRL: DWORD;  Ok, OkCopy: Boolean;
begin
  Path := ParamInfo + '\';
  Ok := False;

  // read Ini setup configuration
  AFile := Path + SC_INI_SETUP;
  If FileExists( AFile ) then begin

    I := TIniFile.Create( AFile );

    InstallDir := I.ReadString( SC_INI_GROUP, REG_APP_FLD, '?' );
    UsrDataDir := I.ReadString( SC_INI_GROUP, REG_USR_FLD, '?' );
    MenuDir    := I.ReadString( SC_INI_GROUP, REG_MNU_FLD, '?' );
    DesktopDir := I.ReadString( SC_INI_GROUP, REG_DSK_ICO, '?' );

    I.Free;

    If Pos( '?', InstallDir + UsrDataDir + MenuDir + DesktopDir ) = 0 then begin

       // AppSetup sends itself on 2nd parameter
       CallerApp := ParamStr( 2 );
       AFile := InstallDir + SC_STP_EXE;

       // If Caller is AppSetup and is not on Install Folder, then copy installer file because will be our launcher app
       If FileExists( CallerApp ) and ( LowerCase( ExtractFileName( CallerApp ) ) = LowerCase( SC_STP_EXE ) ) then begin

          If FileExists( AFile ) then begin

             // Overwrite setup if it is a superior version
             GetVerInfo( AFile, @DWIM, @DWIL );
             GetVerInfo( AFile, @DWRM, @DWRL );
             OkCopy := ( DWRM >= DWIM ) and ( DWRL >= DWIL );

          end else OkCopy := True;

          If OkCopy then begin
             ForceDirectories( InstallDir );
             CopyFile( PChar( CallerApp ), PChar( AFile ), False );
          end;

       end;

       Ok := ExtractFiles( Path + SC_ZIP_FILE ) and CreateShortcuts and RegisterInstall

    end;

  end;

  If Ok then FileCreate( Path + SC_INSTALL_OK )
  else ShowMsg( MSG_ERR );
end;

function  TDMUpdater.ExtractFiles( AZipFile: String ): Boolean;
var       Folder, AFile: String; OkBin, OkDat: Boolean;
begin
  try
    FrmProgress := TFrmProgress.Create( Self );
    UnZipper.ArchiveProgressMeter := FrmProgress.Meter;
    FrmProgress.Show;

    Folder := ChangeFileExt( AZipFile, '' ) + '\';
    Result := False;

    // Extract main zip file to sub folder of same name
    If ExtractFilesLog( AZipFile, Folder, False ) then begin

       // Extract binaries zip file to installation folder
       AFile := Folder + SC_BIN_FILE;
       OkBin := ExtractFilesLog( AFile, InstallDir, True );

       // Extract data zip file to user folder
       AFile := Folder + SC_DAT_FILE;
       OkDat := ExtractFilesLog( AFile, UsrDataDir, True );

       // result is ok when bin and data are succsessfully extracted
       Result := OkBin and OkDat;

    end;

    //FrmProgress.Meter.

  finally
    FrmProgress.Free;
  end;
end;

function TDMUpdater.ExtractFilesLog(AZipFile, Destination: String; KeepLog: Boolean ): Boolean;
var      Log: String;
begin
  Result := False;
  If FileExists ( AZipFile ) then try

     Log := InstallDir + '\' + ChangeFileExt( ExtractFileName( AZipFile ), '.log' );
     ForceDirectories( Destination );

     If KeepLog then begin

        LogFile := TStringList.Create;

        // load log of copied files
        If FileExists( Log ) then LogFile.LoadFromFile( Log )
        else LogFile.Add( ExtractFileName( log ) );

     end else LogFile := nil;

     // extract files
     UnZipper.FileName := AZipFile;
     UnZipper.BaseDirectory := Destination;
     UnZipper.ExtractFiles( '*.*' );

     // save log
     If KeepLog then begin
        LogFile.SaveToFile( Log );
        LogFile.free;
     end;

    Result := True;

  except
    On E: Exception do Result := False;
  end;
end;

function  TDMUpdater.CreateShortcuts: Boolean;
var       App, Lnk : String;
begin
  try
    // create desktop lnk file
    App := InstallDir + SC_APP_EXE;
    Lnk := DesktopDir + SC_APP_LNK;
    CreateLnk( Lnk, App, '' );

    // create menu lnk file
    Lnk := MenuDir + SC_APP_LNK;
    CreateLnk( Lnk, App, '' );

    // create menu uninstall file
    App := InstallDir + SC_UPD_EXE;
    Lnk := MenuDir + SC_UPD_LNK;
    CreateLnk( Lnk, App, '/UNINSTALL' );

    Result := True;
  except
    On E: Exception do Result := False;
  end;
end;

function  TDMUpdater.RegisterInstall: Boolean;
// As an elevated process, updater can write to HKEY_LOCAL_MACHINE
var       R : TRegistry; App: String; DWIM: DWORD; y, m, d: Word; ok: Boolean;
begin
  R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
  R.RootKey := HKEY_LOCAL_MACHINE;

  Ok := R.OpenKey( REG_SOFTWARE, False ) and R.OpenKey( REG_APP_KEY, True );

  If Ok then begin

     // keep track of installation folders & files to remove when needed
     R.WriteString( REG_APP_FLD, InstallDir );
     R.WriteString( REG_USR_FLD, UsrDataDir );
     R.WriteString( REG_DSK_ICO, DesktopDir );
     R.WriteString( REG_MNU_FLD, MenuDir );
     R.WriteString( '', REG_APP_KEY );

     R.Free;

     // Now write on windows uninstall section
     R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
     R.RootKey := HKEY_LOCAL_MACHINE;

     If R.OpenKey( REG_UNINST, False ) and R.OpenKey( REG_APP_KEY, True ) then begin

       R.WriteString( '', REG_APP_KEY );
       R.WriteString( 'Publisher', REG_PUBLISH );
       R.WriteString( 'DisplayName', REG_APP_KEY );

       DecodeDate( Date, y, m, d );
       R.WriteString( 'InstallDate', Format( '%d%2.2d%2.2d', [ y, m, d ] ) );

       R.WriteString( 'InstallLocation', InstallDir );
       R.WriteString( 'UninstallString', SafeParam( InstallDir + SC_UPD_EXE ) + ' /UNINSTALL' );
       R.WriteString( 'ModifyPath', SafeParam( InstallDir + SC_UPD_EXE ) + ' /INSTALL' );

       App := InstallDir + SC_APP_EXE;
       R.WriteString( 'DisplayIcon', App );
       R.WriteString( 'DisplayVersion', GetVerInfo( App, @DWIM, nil ) );
       R.WriteInteger( 'VersionMajor', HiWord( DWIM ));
       R.WriteInteger( 'VersionMinor', LoWord( DWIM ));

       R.WriteString( 'URLInfoAbout', REG_APP_URL);
       R.WriteString( 'URLUpdateInfo', REG_APP_URL);
       R.WriteString( 'HelpLink', REG_APP_URL);

       R.WriteInteger( 'NoModify', 1);
       R.WriteInteger( 'NoRepair', 0);

     end else Ok := False;

     R.Free;

  end;

  If Not Ok then ShowMsg( 'Registry Error' );
  Result := Ok;
end;

procedure TDMUpdater.AppUpdate;
begin
  // ParamInfo is the zip file
  If ExtractFiles( ParamInfo ) then
     FileCreate( ExtractFilePath( ParamInfo ) + SC_INSTALL_OK );
end;

procedure TDMUpdater.AppUnInstall;
const     UNINS = 'Are you sure to uninstall?';
          ERROR = 'Error copying uninstaller';
var       TempFld, AFile: String;
begin
  If ShowMsg( UNINS ) then begin

     If AppSendClose then begin

        TempFld := NewTempFolder( UsrTempDir, 'AppDemo' );
        ForceDirectories( TempFld );
        AFile := TempFld + SC_UPD_EXE;

        If CopyFile( PChar( InstallDir + SC_UPD_EXE ), PChar( AFile ), False ) then
           ShellNewProcess( AFile, '/ZAP', True, False )
        else ShowMsg( ERROR );

     end else ShowMsg( MSG_INMEM );

  end;
end;

procedure TDMUpdater.AppDelete;
const     MSG_UNINS = 'Uninstall %s.';
var       R : TRegistry; Ok: Boolean;
begin
   Sleep( 500 );

   // delete setup wich is not on the log
   DeleteFile( PChar( InstallDir + SC_STP_EXE ));

   // delete files from log
   Ok := DeleteFiles( UsrDataDir, InstallDir + 'data.log' ) and DeleteFiles( InstallDir, InstallDir + 'bin.log' );

   // delete menu folder
   ShellDeleteFolder( MenuDir );

   // delete desktop link
   DeleteFile( PChar( DesktopDir + SC_APP_LNK + '.lnk' ));

   // Delete App Key
   R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
   R.RootKey := HKEY_LOCAL_MACHINE;

   If R.OpenKey( REG_SOFTWARE, False ) then Ok := Ok and R.DeleteKey( REG_APP_KEY )
   else Ok := False;

   R.Free;

   // HKLM - Delete Uninstall Key
   R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
   R.RootKey := HKEY_LOCAL_MACHINE;

   If R.OpenKey( REG_UNINST, False ) then
      Ok := Ok and R.DeleteKey( REG_APP_KEY );

   R.Free;

   If Ok then ShowMsg( Format( MSG_UNINS, [ 'done.'] ) )
   else ShowMsg( Format( MSG_UNINS, [ 'failed.'] ) );
end;

function  TDMUpdater.DeleteFiles(const Folder, log: String ): Boolean;
begin
  Result := True;
  try

     LogFile := TStringList.Create;

     // Delete all copied files
     if FileExists( Log ) then
        LogFile.LoadFromFile( Log );

     While LogFile.Count > 0 do begin
           DeleteFile( PChar( Folder + '\' + LogFile[0] ));
           LogFile.Delete(0);
     end;

     // delete log file too
     DeleteFile( PChar( Log ));

     // if empty -> delete folder
     if DirectoryIsEmpty( Folder ) then
        RemoveDir( Folder );               

     LogFile.Free;

  except
    On E: Exception do Result := False;
  end;
end;


end.
