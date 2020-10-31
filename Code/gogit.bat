g:
cd \projects\covid
git add .
echo "Update for %1"
git commit -m "Update for %~1"
git push
