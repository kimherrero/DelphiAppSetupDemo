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
unit AppConsts;

interface

uses Messages;

const
  { Messages }

  WM_CUSTOM_PERFORM = WM_USER + 123;
  PFM_APP_QUIT      = 1;

  { string constants }

  SC_APP_FOLDER = '\App Setup Demo\';
  SC_APP_CLASS  = 'TFrmSimpleApp';
  SC_STP_EXE    = 'AppSetup.exe';
  SC_APP_EXE    = 'SimpleApp.exe';
  SC_APP_LNK    = 'Simple App Demo';
  SC_UPD_EXE    = 'AppUpdater.exe';
  SC_UPD_LNK    = 'Uninstall AppSetup Demo';

  SC_INI_SETUP  = 'simpleappsetup.ini';
  SC_INI_GROUP  = 'SETTINGS';

  SC_ZIP_FILE   = 'Update.zip';
  SC_BIN_FILE   = 'bin.zip';
  SC_DAT_FILE   = 'data.zip';
  SC_INSTALL_OK = 'done.flag';

  REG_SOFTWARE = 'Software';
  REG_UNINST   = REG_SOFTWARE + '\Microsoft\Windows\CurrentVersion\Uninstall';
  REG_APP_KEY  = 'AppSetupDemo';

  REG_APP_FLD  = 'AppFolder';
  REG_USR_FLD  = 'UsrData';
  REG_MNU_FLD  = 'AppMenu';
  REG_DSK_ICO  = 'DesktopIcon';

  REG_PUBLISH  = 'App creator';
  REG_APP_URL  = 'http://yoururl.com';

  REG_INS_FRM  = 'InstallFrom';
  REG_ZAP_LNK  = 'UninstallLnk';
  REG_ZAP_KEY  = 'UninstallString';

  MSG_INMEM    = 'App is loaded, please shut it down before uninstall.';


implementation


end.
