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
unit WinAppCore;

interface

Uses Classes;


function  LoadPaths: Boolean;

var  InstallDir, UsrDataDir, UsrTempDir, MenuDir, DesktopDir: String;

     KEY_32x64: Word;


const
  KEY_WOW64_64KEY    = $0100;
  KEY_WOW64_32KEY    = $0200;


implementation

uses AppConsts, Windows, SysUtils, Registry;

const

  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_AMD64}
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA64}


function  LoadPaths: Boolean;
var       R : TRegistry;
begin
  R := TRegistry.Create;
  R.RootKey := HKEY_LOCAL_MACHINE;    // Admin
  R.Access  := KEY_READ + KEY_32x64;  // Read Only

  Result := False;

  If R.OpenKey( REG_SOFTWARE + '\' + REG_APP_KEY, False ) then begin

     InstallDir  := R.ReadString( REG_APP_FLD );
     UsrDataDir  := R.ReadString( REG_USR_FLD );
     DesktopDir  := R.ReadString( REG_DSK_ICO );
     MenuDir     := R.ReadString( REG_MNU_FLD );

     R.CloseKey;
     Result := True;

  end;

  R.Free;

end;

function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
type     TGetNativeSystemInfo = procedure (var SystemInfo: TSystemInfo) stdcall;
var      LibraryHandle: HMODULE; _GetNativeSystemInfo: TGetNativeSystemInfo;
begin
  Result := False;
  LibraryHandle := GetModuleHandle(kernel32);

  if LibraryHandle <> 0 then begin

    _GetNativeSystemInfo := GetProcAddress( LibraryHandle, 'GetNativeSystemInfo');

    if Assigned(_GetNativeSystemInfo) then begin

      _GetNativeSystemInfo(SystemInfo);
      Result := True;

    end else GetSystemInfo(SystemInfo);

  end else GetSystemInfo(SystemInfo);
end;

function IsWindows64: Boolean;
var      SystemInfo: TSystemInfo;
begin
  GetNativeSystemInfo(SystemInfo);
  Result := SystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64, PROCESSOR_ARCHITECTURE_AMD64];
end;


initialization
  If IsWindows64 then KEY_32x64 := KEY_WOW64_64KEY
  else KEY_32x64 := KEY_WOW64_32KEY;

  InstallDir := '';
  UsrDataDir := '';
  DesktopDir := '';
  MenuDir    := '';

  UsrTempDir := '';

end.
