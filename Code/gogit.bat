D:
cd "My Drive"
cd "Latest"
erase *.pptx
copy "g:\projects\covid\latest\*.*" .
cd data
erase *.csv
copy "g:\projects\covid\latest\data\*.*" .
g:
cd \projects\covid
git add .
echo "Update for %1"
git commit -m "Update for %~1"
git push
