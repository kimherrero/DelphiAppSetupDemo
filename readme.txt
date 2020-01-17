COMPLETE DELPHI APP INSTALLER DEMO PROJECT: ( 32 Bits ) V1.0. January 2020.

This project covers most of the requisites a modern setup application needs to achieve on Windows 
without the help of any scripting system. It is entirely made from the scratch with Delphi 7, thus
providing you all the freedom you may need.

As I found no reliable ways to run a non elevated process from an elevated one ( the installer )
I tried this workaround in which, any install or update attempt is controlled by a non elevated
process.

Under this schema, elevated privileges are given only to the Updater application, making the
extraction & deletion of files on protected folders possible, as well to write under HKLM registry 
keys, as expected from any installer.

AppSetup looks like an installer but delegates all the elevated tasks to AppUpdater, so when finished,
can effectively launch the new installed Application without undesired inherited administrative rights. 
The key for all this is on the AppSetup's manifest, which doesn't claim for elevated privileges, 
and because of this, nor even get flagged by Windows although the word "setup" is clearly there.

Note: I tried to create this Demo with Delphi standard components only, but dealing with zip
files is a must in these matters, so I had to included two components from Abrevia package:
TABUnzipper & TABMeter. You can download it here: https://sourceforge.net/projects/tpabbrevia/

The demo also comes with an interesting "buildAll" batch file, which creates all resources for the apps
and even the update.zip so you can test all features involving install / update / uninstall tasks.

PROJECT MAIN FEATURES:

- Easily launch standard applications with non elevated privileges after Install or Update.
- Correct registration on Windows. Application appears on Control Panel Add/Remove programs. Repair option enabled.
- Rationalization of application folders: InstallDir, UsrDataDir, UsrTempDir, etc. AllUsers vs CurrentUser.
- Resource management of Zip files as a way to deliver files on different folders.
- Accurate Log of copied & updated files on Program & data folders ( easily extensible ).
- Accurate deletion of copied files. Folders are only removed if not empty. No temporary folders left.

Also:

- File version management: Setup file is updated automatically if running version is higher.
- Basic Conditional Defines management: Differentiate DEVELOPMENT from PRODUCTION versions.
- Create shortlinks on Windows menu and desktop folders.
- Application Auto shut down under installer demand.
- Multi instance execution prevention system.
- Batch file to build all projects & zip files.

PROJECT:

The project is composed by 3 different applications: AppSetup, AppUpdater & SimpleApp.

- AppSetup   : Gathers initial setup info. Acts as a launcher for AppUpdater ( elevated ) and SimpleApp ( non elevated ).
- AppUpdater : Core installer & updater application: Copies & delete files on protected folders and writes on Windows Registry.
- SimpleApp  : A Simple Demo Application showing program & data files and the ability to call for an update.

IMPORTANT:

After building all applications with no errors, you MUST execute "AppBuildAndZip.bat". 
Otherwise, modified exe files will not be Zipped and added as resources in the AppSetup application.

DEBUGING:

Another benefit of this schema is that once the application is installed, you can easily debug from your development folder,
because all configuration folders are taken from the registry, and so, any file location taken from the rationalised folders
is correctly found.

Remember you need to run Delphi as Administrator to debug AppUpdater. Also you need to pass same parameters as if they were sent
from AppSetup. To do so, copy the command line and paste into Run -> Parameters menu on the IDE. It should look something like: 
/INSTALL:C:\Users\Kim\AppData\Local\Temp\AppInstall77C761B0 R:\SetupDemo\AppSetup.exe

RUNNING:

Once AppSetup is executed you'll find "App Setup Demo" folder on Windows Start Menu and Control Panel Add/Remove programs.
Also you will find Registry entries on HKLM\Software\App Setup Demo and HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall

KNOWN ISSUES:

Control Panel always prompts for elevated privileges for registered Applications, so when "Repair" option is choosed AppSetup.exe 
runs elevated, making SimpleApp running elevated too. If that can be a problem, simple deactivate repair option by setting 
NoModify=1 and NoRepair=1 and removing ModifyPath entries on RegisterInstall procedure.


DISCLAIMER: 

All projects and units are provided "AS IS" with no warranty of any kind.

LICENSE:

Feel free to use and change as desired on any project you like. I did this for fun and to improve my own set of applications.
I don't require to be mentioned on your final developments, but I would like to keep credited as creator of this project
if anyone further extends it.

Thanks and enjoy,

Kim Herrero 
January 2020

You can contact me at:
kim@kimherrero.com

