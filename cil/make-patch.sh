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

SCRIPTDIR=`pwd`

#Temporary files location
TEMPHOST=/tmp
TEMPDIR=patched_cil
TEMPORIGDIR=orig_cil

#Copy current and orig CIL to temporary directory
rm -f -r $TEMPHOST/$TEMPDIR
cp -r -f $CILHOME $TEMPHOST/$TEMPDIR
rm -f -r $TEMPHOST/$TEMPORIGDIR
cp -r -f $CILORIG $TEMPHOST/$TEMPORIGDIR

#Remove all files that neither belong to original CIL nor are described in MANIFEST
pushd $TEMPHOST/$TEMPDIR
for fl in `find .`; do
  if [ -f $fl ]; then
    FILENAME=`basename $fl`
    DIRNAME=`dirname $fl`
    MANIFEST=$DIRNAME/.MANIFEST
    if [ ! -e $CILORIG/$fl ]; then
      if [ ! -e $MANIFEST ] || [ -z `grep "$FILENAME" -l $MANIFEST` ]; then
        rm -f $fl
      fi
    fi
  fi
done
popd

#Create patch
cd $TEMPHOST
diff -r -c -N $TEMPORIGDIR/ $TEMPDIR/ > $SCRIPTDIR/$CIL_PATCH_VERSION/diff.log

#Remove temporary files
rm -f -r $TEMPHOST/$TEMPDIR
rm -f -r $TEMPHOST/$TEMPORIGDIR