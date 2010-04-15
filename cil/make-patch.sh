if [ -z "$CILHOME" ]; then 
  echo "CILHOME variable should be set to a reference on folder containing CIL." 
  exit 
fi

if [ -z "$CILORIG" ]; then 
  echo "CILORIG variable should be set to a reference on folder containing original version of CIL." 
  exit 
fi

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
pushd $TEMPHOST/$TEMPDIR > /dev/null
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
popd > /dev/null

#Create patch
cd $TEMPHOST
diff -r -c -N $TEMPORIGDIR/ $TEMPDIR/ 

#Remove temporary files
rm -f -r $TEMPHOST/$TEMPDIR
rm -f -r $TEMPHOST/$TEMPORIGDIR