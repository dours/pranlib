if [ -z "$CILHOME" ]; then 
  echo "CILHOME variable is not set." 
  exit 
fi

if [ -z "$CILORIG" ]; then 
  echo "CILORIG variable is not set." 
  exit 
fi

CIL_PATCH_VERSION="`grep CIL_VERSION_MAJOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_MINOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_REV= $CILHOME/configure | awk -F= '{print $2}'`"

if [ -z "$CIL_PATCH_VERSION" ]; then 
 echo "Cannot determine version of CIL."
 exit
fi

if [ -d "$CIL_PATCH_VERSION" ]; then 
  sleep 0
else
  mkdir $CIL_PATCH_VERSION
fi

#Temporary files location
TEMPDIR=/tmp/patched_cil2

#Copy current CIL to temporary directory
rm -f -r $TEMPDIR
cp -r -f $CILHOME $TEMPDIR

#Remove all files that neither belong to original CIL nor are described in MANIFEST
pushd $TEMPDIR
for fl in `find .`; do
  if [ -f $fl ]; then
    FILENAME=`basename $fl`
    DIRNAME=`dirname $fl`
    MANIFEST=$DIRNAME/.MANIFEST
    if [ ! -e $CILORIG/$fl ]; then
      if [ ! -e $MANIFEST ] || [ -z `grep "$FILENAME" -l $MANIFEST` ]; then
        sudo rm -f $fl
      fi
    fi
  fi
done
popd

#Create patch
diff -r -c -N $CILORIG/ $TEMPDIR/ > $CIL_PATCH_VERSION/diff.log

#Remove temporary files
rm -f -r $TEMPDIR