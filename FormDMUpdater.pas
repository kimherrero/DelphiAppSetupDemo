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
unit FormDMUpdater;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Registry,
  Dialogs, FormDMUnzip, AbBase, AbArcTyp, AbBrowse, AbZBrows, AbUnzper;

type
  TDMUpdater = class(TDMUnzip)
    procedure DataModuleCreate(Sender: TObject);
    procedure UnZipperArchiveItemProgress(Sender: TObject; Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
  private
    { Private declarations }
    LogFileList,
    LogFile: TStringList;
    InstallSize: Integer;
    R : TRegistry;
    procedure AppInstall;
    procedure AppUpdate;
    procedure AppUnInstall;
    procedure AppDelete;
    function  DeleteFiles( const Folder, log: String ): Boolean;
    function  RegisterInstall: Boolean;
    procedure RegisterUpdate;
    function  ExtractFiles( AZipFile: String ): Boolean;
    function  ExtractFilesLog( AZipFile, Destination: String; KeepLog: Boolean ): Boolean;
    procedure ExtractFilesCount;
    function  CreateShortcuts: Boolean;
  public
    { Public declarations }
  end;

var
  DMUpdater: TDMUpdater;

implementation

uses SysCaller, WinAppCore, IniFiles, FormMsg, AppConsts, FormProgress;

{$WARN SYMBOL_PLATFORM OFF}

{$R *.dfm}

procedure TDMUpdater.DataModuleCreate(Sender: TObject);
begin
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

       Ok := ExtractFiles( Path + SC_ZIP_FILE ) and RegisterInstall and CreateShortcuts; 

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

    LogFileList := TStringList.Create;

    // Extract main zip file to sub folder of same name
    If ExtractFilesLog( AZipFile, Folder, False ) then begin

       // 1st : Extract binaries zip file to installation folder
       AFile := Folder + SC_BIN_FILE;
       OkBin := ExtractFilesLog( AFile, InstallDir, True );

       // 2nd : Extract data zip file to user folder
       AFile := Folder + SC_DAT_FILE;
       OkDat := ExtractFilesLog( AFile, UsrDataDir, True );

       // result is ok when bin and data are succsessfully extracted
       Result := OkBin and OkDat;

    end;

    // force repaint
    FrmProgress.Show;
    Application.ProcessMessages;

    // get installation size
    ExtractFilesCount;

  finally
    FrmProgress.Free;
  end;
end;

function TDMUpdater.ExtractFilesLog(AZipFile, Destination: String; KeepLog: Boolean ): Boolean;
var      BaseName, Log: String;
begin
  Result := False;
  If FileExists ( AZipFile ) then try

     ForceDirectories( Destination );

     If KeepLog then begin

        // keep track of log files
        BaseName := ChangeFileExt( ExtractFileName( AZipFile ), '' );
        LogFileList.Add( BaseName + '=' + Destination);

        Log := InstallDir + '\' + BaseName + '.log';
        LogFile := TStringList.Create;

        // load log of copied files
        If Not FileExists( Log ) then begin

           // Add previous extracted setup
           if InstallSize = 0 then LogFile.Add( LowerCase( SC_STP_EXE ));
           // add log file too
           LogFile.Add( ExtractFileName( log ) );

        end else LogFile.LoadFromFile( Log );

     end else LogFile := nil;

     // extract files
     UnzipFile( AZipFile, Destination, '*.*' );

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

procedure TDMUpdater.ExtractFilesCount;
var       Folder, AFile: String; SR: TSearchRec; Size: Integer;
begin
  InstallSize := 0;
  While LogFileList.Count > 0 do begin

        // determine log file name and folder
        AFile  := Copy( LogFileList[0], 1, Pos( '=', LogFileList[0] )-1 );
        Folder := LogFileList.Values[ AFile ];  // files folder not log
        AFile  := InstallDir + AFile + '.log';

        If FileExists( AFile ) then begin

           LogFile := TStringList.Create;
           LogFile.LoadFromFile( AFile );

           if FindFirst( Folder + '\*.*', faArchive, SR) = 0 then  // or faDirectory
           Repeat

             AFile := LowerCase( SR.Name );
             If ((SR.Attr and faArchive) <> 0) and ( LogFile.IndexOf( AFile ) >= 0 ) then begin
                Size := Int64( SR.FindData.nFileSizeHigh) shl Int64(32) + Int64(SR.FindData.nFileSizeLow);  // sRec.Size;
                InstallSize := InstallSize + Size;
             end;

           Until FindNext(SR) <> 0;
           FindClose(SR);
           LogFile.Free;

        end;

        LogFileList.Delete(0);

  end;

  LogFileList.Free;

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
var       ok: Boolean;
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

       R.WriteString( 'InstallSource', ParamStr( 2 ) );  // Setup Caller
       R.WriteString( 'InstallLocation', InstallDir );
       R.WriteString( 'UninstallString', SafeParam( InstallDir + SC_UPD_EXE ) + ' /UNINSTALL' );

       { Don't set ModifyPath if you don't want AppSetup, and therefore, SimpleApp, to be called elevated from Control Panel
         Also, set NoModify = 1 & NoRepair = 1.
       }

       R.WriteString( 'ModifyPath', SafeParam( InstallDir + SC_STP_EXE ) + ' /INSTALL' );
       R.WriteInteger( 'NoModify', 0);
       R.WriteInteger( 'NoRepair', 0);

       R.WriteString( 'URLInfoAbout', REG_APP_URL);
       R.WriteString( 'URLUpdateInfo', REG_APP_URL);
       R.WriteString( 'HelpLink', REG_APP_URL);

       // Register version, date & size
       RegisterUpdate;

     end else Ok := False;

     R.Free;

  end;

  If Not Ok then ShowMsg( 'Registry Error' );
  Result := Ok;
end;

procedure TDMUpdater.RegisterUpdate;
var       App: String; DWIM: DWORD; y, m, d: Word;
begin
   App := InstallDir + SC_APP_EXE;
   R.WriteString( 'DisplayIcon', App );

   // Register current version
   R.WriteString( 'DisplayVersion', GetVerInfo( App, @DWIM, nil ) );
   R.WriteInteger( 'VersionMajor', HiWord( DWIM ));
   R.WriteInteger( 'VersionMinor', LoWord( DWIM ));

   // Register current size in Kb
   R.WriteInteger( 'EstimatedSize', InstallSize div 1024 );

   // Keep track of first & last install
   If R.KeyExists( 'InstallDate' ) and Not R.KeyExists( 'InstallFirst' ) then
      R.WriteString( 'InstallFirst', R.ReadString( 'InstallDate' ));

   DecodeDate( Date, y, m, d );
   R.WriteString( 'InstallDate', Format( '%d%2.2d%2.2d', [ y, m, d ] ) );
end;

procedure TDMUpdater.AppUpdate;
begin
  // ParamInfo is the zip file
  If ExtractFiles( ParamInfo ) then begin

       // Now write on windows uninstall section
     R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
     R.RootKey := HKEY_LOCAL_MACHINE;

     If R.OpenKey( REG_UNINST, False ) and R.OpenKey( REG_APP_KEY, True ) then begin
        RegisterUpdate;
        FileCreate( ExtractFilePath( ParamInfo ) + SC_INSTALL_OK );
     end;

     R.Free

  end;

end;

procedure TDMUpdater.AppUnInstall;
const     UNINS = 'Are you sure to uninstall?';
          ERROR = 'Error copying uninstaller';
var       AFile: String;
begin
  If ShowMsg( UNINS, True ) then begin

     If AppSendClose then begin

        AFile := IncludeTrailingPathDelimiter( UsrTempDir ) + SC_UPD_EXE;

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
   // wait for caller shutdown
   Sleep( 250 );

   // delete files from log
   Ok := DeleteFiles( UsrDataDir, InstallDir + 'data.log' ) and DeleteFiles( InstallDir, InstallDir + 'bin.log' );

   // delete menu folder
   ShellDeleteFolder( MenuDir );

   // delete desktop link
   DeleteFile( PChar( DesktopDir + SC_APP_LNK + '.lnk' ));

   // Delete App Key
   R := TRegistry.Create( KEY_ALL_ACCESS or KEY_32x64 );
   R.RootKey := HKEY_LOCAL_MACHINE;

   If Ok and R.OpenKey( REG_SOFTWARE, False ) then Ok := R.DeleteKey( REG_APP_KEY )
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
