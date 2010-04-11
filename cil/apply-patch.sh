if [ -z "$CILHOME" ]; then 
  echo "CILHOME variable is not set." 
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
  sudo chmod +x $t
done

NEWFILES=`awk '/diff -r -c -N/ { path = $6 } /\*\*\* 0 \*\*\*\*/ { print(path) }' $PATCHFILE |
          sed "s/[a-zA-Z]*\///"`

for nf in $NEWFILES; do
  WAY=`echo $nf | tr "/" "\n"`
 
  cd $CILHOME  
  for x in $WAY
  do
    touch .MANIFEST
    if [ -z `grep "$x\$" -l .MANIFEST` ] && [ -f "./$x" ]; then
      echo $x >> .MANIFEST
    fi
    if [ -d $x ]; then
      cd $x
    fi 
  done
done


