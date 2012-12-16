$OS = 'win32'
$version = "1.0b2"

#./prepare.ps1

strip dist/build/difftimeline/difftimeline.exe

$binaryfile="difftimeline-$VERSION-$OS.zip"
write-zip .\dist\build\difftimeline\difftimeline.exe $binaryfile

# push to intarweb
git branch -D gh-pages
git co -b gh-pages origin/gh-pages
mv $binaryfile binaries/
git add "binaries/$binaryfile"
git commit -m "New binary for $OS (version : $VERSION)"
git push
git co master

