@echo off
set VAR=Destination
for /f "skip=1" %%L in ('wmic logicaldisk where volumename^="Google Drive File Stream" Get Caption') do @call :SetVar %%L

echo Copying data to %Destination%
%Destination%

cd "\My Drive\Latest"
erase *.* /Q
copy "g:\projects\covid\latest\*.*" .

cd "\My Drive\Latest\Data"
erase *.* /Q
copy "g:\projects\covid\latest\data\*.*" .

set Destination="%Destination%\My Drive\Docs"

robocopy "g:\projects\covid\docs" %Destination% /S /R:0 /XJ /NP /XO 

g:
cd \projects\covid\code
goto :EOF

:SetVar
set Label=%1
if NOT [%Label%]==[] set %VAR%=%Label%
goto :EOF

