CIL_PATCH_VERSION="`grep CIL_VERSION_MAJOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_MINOR= $CILHOME/configure | awk -F= '{print $2}'`.`grep CIL_VERSION_REV= $CILHOME/configure | awk -F= '{print $2}'`"

if [ -z "$CIL_PATCH_VERSION" ]; then 
 echo "Cannot determine version of CIL."
 exit
fi

if [ ! -d "$CIL_PATCH_VERSION" ]; then 
  mkdir $CIL_PATCH_VERSION
fi

./make-patch.sh > ./$CIL_PATCH_VERSION/diff.log