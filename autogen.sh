#!/bin/sh

set -x
pushd `dirname $0`
aclocal
autoconf --force
automake --add-missing --copy --foreign
svn info|grep '^Revision:'|awk '{print "$Revision:",$2,"$"}' > VERSION
popd
