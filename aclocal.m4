dnl aclocal.m4 generated automatically by aclocal 1.4-p6

dnl Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl This file was synchronized with template ($Revision: 6 $)
dnl
dnl -*- autoconf -*- macros for OCaml
dnl by Olivier Andrieu and Grigory Batalov
dnl from a configure.in by Jean-Christophe FilliUtre,
dnl from a first script by Georges Mariano
dnl
dnl defines AC_PROG_OCAML that will check the OCaml compiler
dnl and set the following variables :
dnl   OCAMLC        "ocamlc" if present in the path, or a failure
dnl                 or "ocamlc.opt" if present with same version number as ocamlc
dnl   OCAMLOPT      "ocamlopt" (or "ocamlopt.opt" if present), or unset
dnl   OCAMLBEST     either "byte" if no native compiler was found, 
dnl                 "opt" otherwise
dnl   OCAMLDEP      "ocamldep"
dnl   OCAMLLIB      the path to the ocaml standard library
dnl   OCAMLVERSION  the ocaml version number
dnl
dnl   OCAMLMKTOP    
dnl   OCAMLMKLIB    
dnl   OCAMLDOC

AC_DEFUN([AC_PROG_OCAML], [

dnl Checking for OCaml compiler
AC_CHECK_PROG(have_ocamlc, ocamlc, yes, no)
if test "$have_ocamlc" = no; then
    AC_MSG_ERROR(Cannot find ocamlc)
    unset OCAMLC
else
    OCAMLC=ocamlc
fi

dnl Checking OCaml version
AC_MSG_CHECKING(for OCaml version)
OCAMLVERSION=$($OCAMLC -version)
AC_MSG_RESULT($OCAMLVERSION)
if test "$OCAMLVERSION" = ""; then
    AC_MSG_ERROR(OCaml version not set)
fi

dnl Searching for library path
AC_MSG_CHECKING(for OCaml library path)
OCAMLLIB=$($OCAMLC -where)
AC_MSG_RESULT($OCAMLLIB)
if test "$OCAMLLIB" = ""; then
    AC_MSG_ERROR(OCaml library path)
fi

dnl Checking for OCaml native compiler
AC_CHECK_PROG(have_ocamlopt, ocamlopt, yes, no)
if test "$have_ocamlopt" = "no"; then
    AC_MSG_WARN(Cannot find ocamlopt; bytecode compilation only)
    unset OCAMLOPT
else
    OCAMLOPT=ocamlopt
    AC_MSG_CHECKING(for ocamlopt version)
    TMPVERSION=$($OCAMLOPT -version)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_ERROR(ocamlopt version not set; ocamlopt discarded)
	unset OCAMLOPT
    fi
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_WARN(($TMPVERSION) differs from ocamlc; ocamlopt discarded)
	unset OCAMLOPT
    fi
fi

dnl Checking for ocamlc.opt
AC_CHECK_PROG(have_ocamldotopt, ocamlc.opt, yes, no)
if test "$have_ocamldotopt" = "no"; then
    unset OCAMLCDOTOPT
else
    OCAMLCDOTOPT=ocamlc.opt
    AC_MSG_CHECKING(for ocamlc.opt version)
    TMPVERSION=$($OCAMLCDOTOPT -version)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_ERROR(ocamlc.opt version not set; ocamlc.opt discarded)
	unset OCAMLCDOTOPT
    fi
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_RESULT(($TMPVERSION) differs from ocamlc; ocamlc.opt discarded)
	unset OCAMLCDOTOPT
    else
	OCAMLC=$OCAMLCDOTOPT
    fi
fi

dnl Checking for ocamlopt.opt
if test "$have_ocamlopt" = "yes"; then
    AC_CHECK_PROG(have_ocamloptdotopt, ocamlopt.opt, yes, no)
    if test "$OCAMLOPTDOTOPT" = "no"; then
	unset OCAMLOPTDOTOPT
    else
	OCAMLOPTDOTOPT=ocamlopt.opt
	AC_MSG_CHECKING(for ocamlopt.opt version)
	TMPVERSION=$($OCAMLOPTDOTOPT -version)
	AC_MSG_RESULT($TMPVERSION)
	if test "$TMPVERSION" = ""; then
	    AC_MSG_ERROR(ocamlopt.opt version not set; ocamlopt.opt discarded)
	    unset OCAMLOPTDOTOPT
	fi
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(($TMPVERSION) differs from ocamlc; ocamlopt.opt discarded)
	    unset OCAMLOPTDOTOPT
	else
	    OCAMLOPT=$OCAMLOPTDOTOPT
	fi
    fi
fi

dnl Checking for ocamldep
AC_CHECK_PROG(have_ocamldep, ocamldep, yes, no)
if test "$have_ocamldep" = no; then
    AC_MSG_ERROR(Cannot find ocamldep)
    unset OCAMLDEP
else
    OCAMLDEP=ocamldep
fi

dnl Checking for ocamlmktop
AC_CHECK_PROG(have_ocamlmktop, ocamlmktop, yes, no)
if test "$have_ocamlmktop" = no; then
    AC_MSG_WARN(Cannot find ocamlmktop)
    unset OCAMLMKTOP
else
    OCAMLMKTOP=ocamlmktop
fi

dnl Checking for ocamlmklib
AC_CHECK_PROG(have_ocamlmklib, ocamlmklib, yes, no)
if test "$have_ocamlmklib" = no; then
    AC_MSG_WARN(Cannot find ocamlmklib)
    unset OCAMLMKLIB
else
    OCAMLMKLIB=ocamlmklib
fi

dnl Checking for ocamldoc
AC_CHECK_PROG(have_ocamldoc, ocamldoc, yes, no)
if test "$have_ocamldoc" = no; then
    AC_MSG_WARN(Cannot find ocamldoc)
    unset OCAMLDOC
else
    OCAMLDOC=ocamldoc
fi

dnl Get the C compiler used by ocamlc
if test "$have_ocamlopt" ; then
    AC_MSG_CHECKING(for OCaml C compiler)
    touch conftest.c
    OCAMLCC=$($OCAMLC -verbose conftest.c 2>&1 | awk '/^\+/ {print $[]2 ; exit}')
    AC_MSG_RESULT($OCAMLCC)
    AC_CHECK_PROG(have_ocamlcc, $OCAMLCC, yes, no)
    if test "$have_ocamlcc" = "no"; then
	AC_MSG_WARN(Cannot find OCaml C compiler ($OCAMLCC); bytecode compilation only)
	unset OCAMLOPT
    fi
fi

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(OCAMLDOC)
])

dnl
dnl macro AC_PROG_OCAML_TOOLS will check OCamllex and OCamlyacc :
dnl   OCAMLLEX      "ocamllex" or "ocamllex.opt" if present
dnl   OCAMLYACC     "ocamlyac"
AC_DEFUN([AC_PROG_OCAML_TOOLS], [

dnl Checking for ocamllex
AC_CHECK_PROG(have_ocamllex, ocamllex, yes, no)
if test "$have_ocamllex" = no; then
    AC_MSG_ERROR(Cannot find ocamllex)
    unset OCAMLLEX
else
    OCAMLLEX=ocamllex
    AC_CHECK_PROG(have_ocamllexdotopt, ocamllex.opt, yes, no)
    if test "$have_ocamllexdotopt" = "no"; then
	unset OCAMLLEXDOTOPT
    else
	OCAMLLEXDOTOPT=ocamllex.opt
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
fi

dnl Checking for ocamlyacc
AC_CHECK_PROG(have_ocamlyacc, ocamlyacc, yes, no)
if test "$have_ocamlyacc" = no; then
    AC_MSG_ERROR(Cannot find ocamlyacc)
    unset OCAMLYACC
else
    OCAMLYACC=ocamlyacc
fi

AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
])

dnl
dnl AC_PROG_CAMLP4 checks for Camlp4
AC_DEFUN([AC_PROG_CAMLP4], [
AC_REQUIRE([AC_PROG_OCAML])
dnl Checking for camlp4
AC_CHECK_PROG(have_camlp4, camlp4, yes, no)
if test "$have_camlp4" = no; then
    AC_MSG_ERROR(Cannot find camlp4)
    unset CAMLP4
else
    CAMLP4=camlp4
    AC_MSG_CHECKING(for camlp4 version)
    TMPVERSION=$($CAMLP4 -version)
    AC_MSG_RESULT($TMPVERSION)
    if test "$TMPVERSION" = ""; then
	AC_MSG_ERROR(camlp4 version not set; camlp4 discarded)
	unset CAMLP4
    fi
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_ERROR(($TMPVERSION) differs from ocamlc; camlp4 discarded)
	unset CAMLP4
    else
dnl Checking for Camlp4o
        AC_CHECK_PROG(have_camlp4o, camlp4o, yes, no)
	if test "$have_camlp4o" = no; then
	    unset CAMLP4O
	else
	    CAMLP4O=camlp4o
	fi
dnl Checking for Camlp4r
	AC_CHECK_PROG(have_camlp4r, camlp4r, yes, no)
	if test "$have_camlp4r" = no; then
	    unset CAMLP4R
	else
	    CAMLP4R=camlp4r
	fi
    fi
fi
AC_SUBST(CAMLP4)
AC_SUBST(CAMLP4O)
AC_SUBST(CAMLP4R)
])

dnl
dnl macro AC_PROG_FINDLIB will check for the presence of
dnl   ocamlfind if configure is called with --with-findlib
AC_DEFUN([AC_PROG_FINDLIB], [
AC_ARG_WITH(findlib,[  --without-findlib       do not use findlib package system],
  use_findlib="$withval")
dnl Checking for ocamlfind
if ! test "$use_findlib" = no ; then 
	AC_CHECK_PROG(have_ocamlfind, ocamlfind, yes, no)
	if test "$have_ocamlfind" = no; then
	    AC_MSG_WARN(ocamlfind not found)
	    unset OCAMLFIND
	else
	    OCAMLFIND=ocamlfind
	fi
else
	unset OCAMLFIND
fi
AC_SUBST(OCAMLFIND)
])

dnl
dnl macro AC_PROG_OCAMLDSORT will check OCamlDSort :
dnl   OCAMLDSORT    "ocamldsort"
AC_DEFUN([AC_PROG_OCAMLDSORT], [
AC_CHECK_PROG(have_ocamldsort, ocamldsort, yes, no)
if test "$have_ocamldsort" = no; then
    AC_MSG_WARN(Cannot find ocamldsort. Using echo.)
    OCAMLDSORT=echo
else
    OCAMLDSORT=ocamldsort
fi
AC_SUBST(OCAMLDSORT)
])


dnl
dnl AC_ARG_OCAML_SITELIBR adds a --with-sitelib option
dnl 1 -> where to install packages
AC_DEFUN([AC_ARG_OCAML_SITELIB], [
AC_ARG_WITH(sitelib,[  --with-sitelib=DIR      specify installation directory],
    SITELIBDIR="$withval")
AC_SUBST(SITELIBDIR)
])

dnl
dnl AC_CHECK_OCAML_MODULE looks for a modules in a given path
dnl 1 -> module to check
dnl 2 -> capitalized names to use with open and for printing
dnl 3 -> extra searching dirs
AC_DEFUN([AC_CHECK_OCAML_MODULE], [
for module in $2; do
    AC_MSG_CHECKING(for $1 ($module))
cat > conftest.ml <<EOF
open $module
EOF
dnl Check module "as is"
    if $OCAMLC -c $MODULE_INCLUDES conftest.ml > /dev/null 2>&1 ; then
	result="found"
	found=yes
    else
	unset found
        if ! test "$use_findlib" = no ; then 
dnl Query package via ocamlfind
	    if check_inc=`$OCAMLFIND query -i-format $1 2>/dev/null`; then
		if $OCAMLC -c $MODULE_INCLUDES $check_inc conftest.ml > /dev/null 2>&1 ; then
		    MODULE_INCLUDES="$MODULE_INCLUDES $check_inc"
		    result="adding $check_inc"
		    found=yes
		fi
	    fi
	fi
	if ! test "$found"; then
dnl Search through specified dirs
	    for check_dir in $3 $SITELIBDIR/$1; do
		if $OCAMLC -c -I $check_dir conftest.ml > /dev/null 2>&1 ; then
		    MODULE_INCLUDES="$MODULE_INCLUDES -I $check_dir"
		    result="adding -I $check_dir"
		    found=yes
		    break
		fi
	    done
	fi
    fi
	
    
    if test "$found" ; then
		if ! $OCAMLC -o conftest $MODULE_INCLUDES $1.cma conftest.cmo > /dev/null 2>&1 ; then
		    AC_MSG_RESULT($1.cma not found)
		    AC_MSG_ERROR(Required library $1 not found)
		else
		    if ! echo "$EXTRA_CMA" | grep "$1.cma" > /dev/null 2>&1 ; then
			EXTRA_CMA="$EXTRA_CMA $1.cma"
		    fi
		    AC_MSG_RESULT($result)
		fi
    else
		AC_MSG_RESULT(not found)
		AC_MSG_ERROR(Required module $module not found in library $1)
    fi
done
AC_SUBST(MODULE_INCLUDES)
AC_SUBST(EXTRA_CMA)
])


dnl
dnl AC_CHECK_CAMLP4_MODULE looks for a module in a given path
dnl 1 -> module to check
dnl 2 -> file name to use with load
dnl 3 -> extra searching dirs
AC_DEFUN([AC_CHECK_CAMLP4_MODULE], [
AC_MSG_CHECKING(for $1 ($2))
cat > conftest.ml <<EOF
#load "$2";
EOF
dnl Check module "as is"
if $CAMLP4R $PARSER_INCLUDES conftest.ml > /dev/null 2>&1 ; then
    AC_MSG_RESULT(found)
else
    unset found
    if ! test "$use_findlib" = no ; then 
dnl Query package via ocamlfind
	if check_inc=`$OCAMLFIND query -i-format $1 2>/dev/null`; then
	    if $CAMLP4R $PARSER_INCLUDES $check_inc conftest.ml > /dev/null 2>&1 ; then
		AC_MSG_RESULT(adding $check_inc)
		PARSER_INCLUDES="$PARSER_INCLUDES $check_inc"
		found=yes
	    fi
	fi
    fi
    if ! test "$found"; then
dnl Search through specified dirs
	for check_dir in $3 $SITELIBDIR/$1; do
	    if $CAMLP4R -I $check_dir conftest.ml > /dev/null 2>&1 ; then
		found=yes
		break
	    fi
	done
	if test "$found" ; then
	    AC_MSG_RESULT(adding -I $check_dir)
	    PARSER_INCLUDES="$PARSER_INCLUDES -I $check_dir"
	else
	    AC_MSG_RESULT(not found)
	    AC_MSG_ERROR(Required module $1 not found)
	fi
    fi
fi
AC_SUBST(PARSER_INCLUDES)
])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version}, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version}, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

# Copyright 2002  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
AC_DEFUN([AM_AUTOMAKE_VERSION],[am__api_version="1.4"])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION so it can be traced.
# This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
	 [AM_AUTOMAKE_VERSION([1.4-p6])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN([AM_MISSING_PROG],
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

# Define a conditional.

AC_DEFUN([AM_CONDITIONAL],
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

