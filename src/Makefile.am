# type in sources here
SOURCES = binomialHeap.ml pairMatching.ml dOT.ml tree.ml digraph.ml breakCycles.ml cFG.ml order.ml generate.ml dFST.ml pathCovering.ml cFO.ml dominance.ml region.ml forest.ml loops.ml alts.ml regularCFG.ml semilattice.ml programView.ml dFAEngine.ml hammocks.ml aliasView.ml dFACommon.ml dFATestCommon.ml rd.ml lv.ml dDG.ml dDAInfo.ml dDGStructure.ml dDA.ml
MLI =  binomialHeap.mli pairMatching.mli dOT.mli tree.mli digraph.mli breakCycles.mli cFG.mli order.mli generate.mli dFST.mli pathCovering.mli cFO.mli dominance.mli region.mli forest.mli loops.mli alts.mli regularCFG.mli semilattice.mli programView.mli dFAEngine.mli hammocks.mli aliasView.mli dFACommon.mli dFATestCommon.mli rd.mli lv.mli dDG.mli dDAInfo.mli dDGStructure.mli dDA.mli

include $(top_srcdir)/m4/Makefile.am.vars

# overwrite some variables

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets

install-data-am: $(TARGET)
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" != ""; then \
	    $(mkinstalldirs) $(SITELIBDIR); \
	    $(OCAMLFIND) install -destdir $(SITELIBDIR) $(PACKAGE) $(TARGET); \
	else \
	    $(OCAMLFIND) install $(PACKAGE) $(TARGET); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    installdir=$(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    installdir=$(SITELIBDIR)/$(PACKAGE); \
	fi; \
	$(mkinstalldirs) $$installdir; \
	for i in $(TARGET); do \
	    $(INSTALL_DATA) $$i $$installdir/$$i; \
	done
endif

#install-exec-am: $(BYTE) $(NATIVE)
#	$(mkinstalldirs) ${bindir}
#	$(INSTALL_PROGRAM) $(BYTE) ${bindir}/$(BYTE)
#	$(INSTALL_PROGRAM) $(NATIVE) ${bindir}/$(PACKAGE)$(EXEEXT)

uninstall-am:
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" = ""; then \
	    $(OCAMLFIND) remove $(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    rm -fR $(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
endif
#	rm -f ${bindir}/$(BYTE) ${bindir}/$(PACKAGE)$(EXEEXT)

include $(top_srcdir)/m4/Makefile.am.rules
