TESTS = test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test013 test014 test015 test016 # test012

TESTS_ENVIRONMENT = $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += ostap.cmo $(PACKAGE).cma

# OCaml parser call with flags
OFLAGS += -rectypes
BFLAGS += -rectypes

AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pa_log.cmo -LOG"

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

TARGET = 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log

include $(top_srcdir)/m4/Makefile.am.targets

# Rules to build tests
%: %.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(PXFLAGS) -o $@$(EXEEXT) $<

include $(top_srcdir)/m4/Makefile.am.rules
