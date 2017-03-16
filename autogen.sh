#!/bin/bash

set -x
pushd `dirname $0`
aclocal-1.9
autoconf --force
automake-1.9 --add-missing --copy --foreign
svn info|grep '^Revision:'|awk '{print "$Revision:",$2,"$"}' > VERSION
popd
