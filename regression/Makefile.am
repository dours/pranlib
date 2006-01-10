TESTS = test001 test002 test003 test004 test005 test006 test007

TESTS_ENVIRONMENT = $(srcdir)/test.sh

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
#AM_P4FLAGS = -pp "$(CAMLP4R) $(PARSER_INCLUDES)"

# bytecode compiling flags
AM_BFLAGS = -w p $(MODULE_INCLUDES) $(DEBUG)
# native (optimized) compiling flags
AM_OFLAGS = -w p $(MODULE_INCLUDES) $(DEBUG)

clean-am:
	rm -f $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
	rm -f $(addsuffix $(EXEEXT), $(TESTS))
	rm -f *.diff *.log

test001: test001.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test002: test002.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test003: test003.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test004: test004.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test005: test005.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test006: test006.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 

test007: test007.ml
	$(OCAMLC) ocamlgraph.cma typeutil.cma pranlib.cma $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $< 
