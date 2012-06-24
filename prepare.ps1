function packAdd( $folder, $version )
{
   pushd
   cd $folder;
   make pack;
   popd

   $packFile = "$folder/$folder-${version}.tar.gz";
   echo $packFile
   cabal-dev add-source $packFile
   rm $packFile
}

packAdd 'hit-simple' '0.3'
packAdd 'ClosureExternalProducer' '0.1'

cabal-dev install

