#!/bin/sh

set -x
aclocal -I config -I m4
autoconf --force
automake --add-missing --copy --foreign
