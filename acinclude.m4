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
