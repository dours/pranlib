#!/bin/sh

set -x
aclocal
autoconf --force
automake --add-missing --copy --foreign
