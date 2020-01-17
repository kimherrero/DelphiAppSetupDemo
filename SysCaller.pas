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
unit SysCaller;

interface

uses Classes, Forms, Windows;

type

  TSystemFolders = ( sfPrograms, sfUsrDesktop, sfAppData, sfUsrStartMenu, sfUsrStartup, sfAllDesktop, sfAllData,
                     sfAllStartMenu, sfAllStartup, sfUsrData, sfWindows, sfSystem, sfTemp, sfCommonAppData );

  function  ShellNewProcess( ProgramName, Params : String; Visible, Wait: Boolean): Boolean;
  function  ShellDeleteFolder( Folder: String ): Boolean;

  function  GetSystemFolderName( IdFolder: TSystemFolders ): String;
  function  GetSpecialFolderName( CSIDL_FOLDER: Integer ): String;
  function  GetVerInfo( const Module: String; lpDWMS, lpDWLS: PDWORD ): String;

  procedure AtomFindCreate( const App: String );
  function  AtomFindDelete( const App: String; Delete: Boolean ): Boolean;

  procedure ExtractResource(const ResName, FileName: String; const Instance: Integer = 0);
  function  BrowseDialog(const Title, InitialDir: string; const Flag: integer = 0): string;
  procedure CreateLnk( LNKName, LNKLink: string; const Params: String = '' );

  function  NewTempFolder( const Path, Prefix: String ): String;
  function  GetFileStringList( Path: String ): TStringList;
  function  DirectoryIsEmpty( const Path: String ): Boolean;
  function  DeleteFolder( const Path: string): Boolean;

  function  GetParamInfo( var ParamInfo: String; const ParamNum: Integer = 1 ): String;
  function  SafeParam( const Param: String ): String;

  function  SetForegroundWindowClass( ClassName: String): Boolean;
  function  IsAdministrator: Boolean;


implementation

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

uses WinAppCore, Messages, SysUtils, ComObj, ActiveX, ShlObj, ShellApi, Registry, FileCtrl, StrUtils;

var  lg_StartFolder: String; // BrowseForFolderCallBack;

const
    SH_FOLDERS_ARRAY: array[ TSystemFolders ] of Integer = ( 38, 0, 28, 2, 7, 25, 46, 23, 24, 5, 0, 0, 0, $23 );

{   (CSIDL_DESKTOP,-1, CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES,
    CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU, CSIDL_DESKTOPDIRECTORY,
    CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD, CSIDL_FONTS, CSIDL_TEMPLATES, 0, 0, 0);
    CSIDL_COMMON_APPDATA needs admin
}


procedure AtomFindCreate( const App: String );
var       AtomText  : array[0..30] of char; FoundAtom : TAtom;
begin
  StrFmt(AtomText, 'App%sIsLoaded', [ App ]);
  FoundAtom := GlobalFindAtom( AtomText );
  If (FoundAtom = 0) then
     GlobalAddAtom(AtomText);
end;

function  AtomFindDelete( const App: String; Delete: Boolean ): Boolean;
var       AtomText  : array[0..30] of char; FoundAtom : TAtom;
begin
  StrFmt(AtomText, 'App%sIsLoaded', [ App ]);
  FoundAtom := GlobalFindAtom( AtomText );
  Result := (FoundAtom <> 0);
  If Result and Delete then
     GlobalDeleteAtom(FoundAtom);
end;

function ShellExec( FileName, Params: string; Visible: Boolean ): THandle;
var      SH: Integer;
begin
  Filename := SafeParam( Filename );

  If Visible then SH := SW_SHOW
  else SH := SW_HIDE;

  Result := ShellExecute( Application.MainForm.Handle, nil, PChar( FileName ), PChar( Params ), nil, SH);
end;

function  ShellExecAndWait(const FileName, Params: string; Visible, Wait : Boolean ): Boolean;
var       zFileName, zParams, zDir, zVerb: array[0..MAX_PATH] of Char;
var       ShellInfo: TShellExecuteInfo; E: DWORD;
begin
  FillChar(ShellInfo, SizeOf(ShellInfo), 0);
  StrPCopy(zFileName, FileName);
  StrPCopy(zParams, Params);
  StrPCopy(zDir, ExtractFileDir( FileName ));
  zVerb := ''; //StrPCopy( zVerb, 'runas' );

  ShellInfo.cbSize := sizeof(TSHELLEXECUTEINFO);
  ShellInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShellInfo.lpVerb := zVerb;
  ShellInfo.lpFile := zFileName;
  ShellInfo.lpParameters := zParams;
  ShellInfo.lpDirectory := zDir;

  If Visible then ShellInfo.nShow := SW_SHOW
  else ShellInfo.nShow := SW_HIDE;

  Result := ShellExecuteEx(@ShellInfo);
  if Result then begin

     If Wait then
        while WaitForSingleObject(ShellInfo.hProcess, INFINITE) <> WAIT_OBJECT_0 do
              Application.ProcessMessages;

  end else begin

      E := GetLastError;
      If E > 0 then
         RaiseLastOSError;

  end;

  CloseHandle(ShellInfo.hProcess);
end;

function  ShellNewProcess( ProgramName, Params : String; Visible, Wait: Boolean): Boolean;
var       StartInfo : TStartupInfo; ProcInfo : TProcessInformation; E : DWORD;
begin
  If Win32MajorVersion < 6 then begin

      FillChar(StartInfo,SizeOf(TStartupInfo),#0);
      FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
      StartInfo.cb := SizeOf(TStartupInfo);

      Result := CreateProcess(nil, PChar(ProgramName + ' ' + Params), nil, nil, False,
                CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS, nil,
                PChar( ExtractFileDir( ProgramName ) ), StartInfo, ProcInfo);

      if Result then begin

         if Wait then
            WaitForSingleObject(ProcInfo.hProcess, INFINITE);

      end else begin

          E := GetLastError;
          If E > 0 then
             RaiseLastOSError;

      end;

      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);

  end else Result := ShellExecAndWait( ProgramName, Params, Visible, Wait );

end;

function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);
  if (GetForegroundWindow <> hwnd) then begin

    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
       ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
       ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
       (Win32MinorVersion > 0)))) then begin

       Result := False;

       ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
       ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);

       if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then begin
          BringWindowToTop(hwnd); // IE 5.5 related hack
          SetForegroundWindow(hwnd);
          AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
          Result := (GetForegroundWindow = hwnd);
       end;

       if not Result then begin
          // Code by Daniel P. Stasinski
          SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
          BringWindowToTop(hwnd); // IE 5.5 related hack
          SetForegroundWindow(hWnd);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
       end;

    end else begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);

  end else Result := True
end;

function  SetForegroundWindowClass( ClassName: String): Boolean;
var       H: THandle;
begin
  H := FindWIndow( PChar( ClassName ), nil );
  If (H <> 0) then Result := ForceForegroundWindow( H )
  else Result := False;
end;

function DeleteFolder( const Path: string): Boolean;
var      sRec: TSearchRec;  t: string;
begin
  Result := False;
  if DirectoryExists(Path) then

  try
    t := IncludeTrailingPathDelimiter(Path);
    if FindFirst(t + '*.*', faArchive or faDirectory, sRec) = 0 then begin

      Repeat

        if ((sRec.Attr and faDirectory) <> 0) and (sRec.Name[1] <> '.') then DeleteFolder( t + sRec.Name)
        else DeleteFile(t + sRec.Name);

      Until FindNext(sRec) <> 0;
      FindClose(sRec);

    end;

    Result := Windows.RemoveDirectory(PChar(t));

  except
  end;

end;

function ShellDeleteFolder( Folder: String ): Boolean;
var      Shfi: TSHFileOpStruct;
begin
  Result := True;
  if not FileExists( Folder ) and not DirectoryExists( Folder ) then Exit;
  shfi.Wnd   := Application.Handle;
  shfi.wFunc := FO_DELETE;
  shfi.pFrom := PChar( Folder + #0#0);
  shfi.pTo   := PChar(#0#0);
  shfi.fFlags := FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI; // or FOF_ALLOWUNDO  or FOF_NOCONFIRMMKDIR
  Result := ShFileOperation(shfi) = 0;
end;

Function GetSysDir( Kind : Integer ): String;
var      ch: Array[0..MAX_PATH] Of Char; num: Integer;
const    Root = 'C:';
begin
  Case Kind of

    0 : If GetWindowsDirectory( @ch, MAX_PATH ) = 0 then Ch := Root;

    1 : If GetSystemDirectory( @ch, MAX_PATH ) = 0 then Ch := Root;

    2 : begin
        num := GetTempPath( MAX_PATH, @ch );
        If num = 0 then Ch := Root
        else Ch[num-1] := Chr(0);
    end;

  end;

  Result := String( ch );
end;

function GetSystemFolderName( IdFolder: TSystemFolders ): String;
begin
  Case IdFolder Of

    sfPrograms, sfUsrDesktop, sfAppData, sfUsrStartMenu, sfUsrStartup,
    sfAllDesktop, sfAllData, sfAllStartMenu,
    sfAllStartup, sfUsrData, sfCommonAppData: Result := GetSpecialFolderName( SH_FOLDERS_ARRAY[ IdFolder ] );

    sfWindows  : Result := GetSysDir( 0 );
    sfSystem   : Result := GetSysDir( 1 );
    sfTemp     : Result := GetSysDir( 2 );

  end;
end;

function GetSpecialFolderName( CSIDL_FOLDER: Integer ): String;
var      TempPath: array[0..MAX_PATH] of Char; ItemIDList: PItemIDList;
begin
  If SHGetSpecialFolderLocation( 0, CSIDL_FOLDER, ItemIdList ) = NOERROR then begin
     SHGetPathFromIDList(ItemIDList,TempPath);
     Result := String( TempPath );
  end else Result := '';
end;

function  GetVerInfo( const Module: String; lpDWMS, lpDWLS: PDWORD ): String;
var       intSize, intTemp: DWORD; pchBuff: PChar;
          recFixedFileInfo: PVSFixedFileInfo;
begin
  intSize := GetFileVersionInfoSize( PChar( Module ), intTemp ) ;
  Result  := '';

  if intSize <> 0 then begin

    pchBuff := strAlloc(intSize) ;

    if GetFileVersionInfo( PChar( Module ), 0, intSize, pchBuff ) then begin

       if VerQueryValue( pchBuff, '\', Pointer( recFixedFileInfo ), intSize) then begin

          If Assigned( lpDWMS ) then lpDWMS^ := recFixedFileInfo^.dwFileVersionMS;
          If Assigned( lpDWLS ) then lpDWLS^ := recFixedFileInfo^.dwFileVersionLS;

          Result := Format( '%d.%d.%d.%d', [
            HIWORD( recFixedFileInfo^.dwFileVersionMS ), LOWORD( recFixedFileInfo^.dwFileVersionMS ),
            HIWORD( recFixedFileInfo^.dwFileVersionLS ), LOWORD( recFixedFileInfo^.dwFileVersionLS ) ] );

       end;

    end;

    StrDispose( pchBuff );

  end;

end;

procedure ExtractResource(const ResName, FileName: String; const Instance: Integer = 0);
var       Resource: TResourceStream; N: Integer;
begin
  try
    If Instance = 0 Then N := MainInstance
    else N := Instance;

    Resource := TResourceStream.Create( N, ResName, RT_RCDATA );
    ForceDirectories( ExtractFileDir( FileName ) );
    If FileExists( FileName ) then DeleteFile( FileName );
    Resource.SaveToFile( FileName );
  finally
    Resource.Free;
  end;
end;

procedure CreateLnk( LNKName, LNKLink: string; const Params: String = '' );
var
  AnObj: IUnknown;
  ShLink: IShellLink;
  PFile: IPersistFile;
  WFileName: WideString;
begin  // access to the two interfaces of the object
  ForceDirectories( ExtractFileDir( LnkName ) );
  AnObj := CreateComObject (CLSID_ShellLink);
  ShLink := AnObj as IShellLink;
  PFile := AnObj as IPersistFile;  // get the name of the application file

  ShLink.SetPath( PChar( LNKLink ) );
  ShLink.SetArguments( PChar( Params ) );
  ShLink.SetWorkingDirectory( PChar( ExtractFilePath (LNKLink) ) );

  // save the file, using a WideString!
  WFileName := LnkName + '.lnk';
  PFile.Save (PWChar (WFileName), False);
end;

function IsUserAnAdmin(): BOOL; external shell32;

function IsAdministrator: Boolean;
begin
  Result := IsUserAnAdmin;
end;

function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
     SendMessage(Wnd,BFFM_SETSELECTION, 1, Integer(@lg_StartFolder[1]));
  result := 0;
end;

function BrowseDialog(const Title, InitialDir: string; const Flag: integer = 0): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
  F: Integer;
begin
  If Flag = 0 then F := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE
  else F := Flag;

  lg_StartFolder := InitialDir;
  Result:='';

  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := F;
    lpfn := BrowseForFolderCallBack;
  end;

  lpItemID := SHBrowseForFolder(BrowseInfo);

  if lpItemId <> nil then begin
     SHGetPathFromIDList(lpItemID, TempPath);
     Result := TempPath;
     GlobalFreePtr(lpItemID);
  end;
end;

function  GetFileStringList( Path: String ): TStringList;
var       SR: TSearchRec; S: String; n: Integer;
begin
  Result := TStringList.Create;
  n  := FindFirst( Path, faAnyFile, SR );
  While n = 0 do begin
        Result.Add( SR.FindData.cFileName );
        n := FindNext( SR );
  end;

  FindClose( SR );
  n := 0;

  While (n < Result.Count) do begin
    S := Result[n];
    If S[1] = '.' then Result.Delete( n )
    else Inc( n );
  end;

end;

function  NewTempFolder( const Path, Prefix: String ): String;
begin
  Randomize;
  Repeat
    Result := IncludeTrailingPathDelimiter( Path ) + Prefix + IntToHex( Random( MAXLONG ), 8 );
  Until Not DirectoryExists( Result );
  Result := Result + '\';
end;

function  DirectoryIsEmpty( const Path: String ): Boolean;
var       SL: TStringList;
begin
  SL := GetFileStringList( IncludeTrailingPathDelimiter( Path ) + '*.*' );
  Result := ( SL.Count = 0 );
  SL.Free;
end;

function  GetParamInfo( var ParamInfo: String; const ParamNum: Integer = 1 ): String;
var       p: Integer;
begin
  Result := ParamStr( ParamNum );
  ParamInfo := '';
  p := Length( Result );

  If p > 0 then begin

     If (Result[p] = '"') then Delete( Result, p, 1 );
     If (p > 1) and (Result[1] = '"') then Delete( Result, 1, 1 );

     p := Pos( ':', Result );
     If p > 0 then begin
        ParamInfo := Copy( Result, p+1, 256 );
        Result    := Copy( Result, 1, p-1 );
     end;

  end;
end;

function  SafeParam( const Param: String ): String;
begin
  if Pos( ' ', Param ) > 0 then Result := '"' + Param + '"'
  else Result := Param;
end;


initialization
  UsrTempDir := GetSystemFolderName( sfTemp );

end.
