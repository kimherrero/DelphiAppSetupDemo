rem Batch file to build all projects & create zip files
rem projects folder is supposed to be on R:\SetupDemo, Delphi on C:\Delphi7 and winrar on C:\Program Files\WinRAR
rem -$Q / Quiet compilation ( no extra info )
rem -B  / Build all units

c:
cd "\Program Files\WinRAR"

r:
cd \SetupDemo

del *.exe
del .\Resources\*.zip
del .\update\*.zip

rem Step 1 : Build update version and zip
"C:\Delphi7\Bin\DCC32.EXE" SimpleApp.dpr -B -Q -DPRODUCTION;CUSTOM
c:winrar a -afzip .\update\bin.zip SimpleApp.exe

cd images
c:winrar a -afzip ..\Resources\data.zip D*.jpg
c:winrar a -afzip ..\update\data.zip U*.jpg

cd ..\update
c:winrar a -afzip update.zip *.zip
del bin.zip
del data.zip
cd..

rem Step 2 : Build install version and zip
"C:\Delphi7\Bin\DCC32.EXE" SimpleApp.dpr -B -Q -DPRODUCTION 
"C:\Delphi7\Bin\DCC32.EXE" AppUpdater.dpr -B -Q

cd Resources
c:winrar a -afzip bin.zip ..\*.exe
c:winrar a -afzip AppRes.zip *.zip
del bin.zip
del data.zip
cd..

"C:\Delphi7\Bin\BRCC32.EXE" AppDemo.rc -r
"C:\Delphi7\Bin\DCC32.EXE" AppSetup.dpr -B -Q 

pause