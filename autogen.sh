#!/bin/sh

aclocal
autoconf --force
automake --add-missing --copy --foreign
