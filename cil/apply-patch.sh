if [ -z "$CILHOME" ]; then 
  echo "CILHOME variable should be set to a reference on folder containing CIL." 
  exit 
fi

CIL_PATCH_VERSION="`grep CIL_VERSION_MAJOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_MINOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_REV= $CILHOME/configure | awk -F= '{print $2}'`"

if [ -d "$CIL_PATCH_VERSION" ]; then 
 echo "CIL version is: $CIL_PATCH_VERSION."
else 
 echo "Patch for CIL version $CIL_PATCH_VERSION not found." 
 exit
fi

PATCHFILE="`pwd`/$CIL_PATCH_VERSION/diff.log"
PATCH_FLAGS="$PATCH_FLAGS -p1"

patch $PATCH_FLAGS -d $CILHOME -i $PATCHFILE

for t in `find $CILHOME -name "*.sh"` ; do
  chmod +x $t
done

#List of files that are not present in the original CIL, but are delivered with patch
NEWFILES=`awk '/diff -r -c -N/ { path = $6 } /\*\*\* 0 \*\*\*\*/ { print(path) }' $PATCHFILE |
          sed "s/[a-zA-Z0-9_-]*\///"`

for nf in $NEWFILES; do
  WAY=`echo $nf | tr "/" "\n"`
 
  cd $CILHOME  
  for x in $WAY
  do
    if [ -f "./$x" ]; then
      # Create record in MANIFEST file describing new file delivered with patch
      touch .MANIFEST
      if [ -z `grep "$x\$" -l .MANIFEST` ]; then
        if [ ! -s .MANIFEST ]; then
          echo $x >> .MANIFEST      
        else
          cp .MANIFEST .MANIFEST.TMP
          sed "\$a$x" .MANIFEST.TMP > .MANIFEST
          rm .MANIFEST.TMP
        fi
      fi 
    elif [ -d $x ]; then
      cd $x
    fi 
  done
done


