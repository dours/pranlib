PARSER_INCLUDES += -I $(OCAMLLIB)/camlp4
# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
#AM_P4FLAGS = -pp "$(CAMLP4R) $(PARSER_INCLUDES)"

.PHONY: doc
# check if style.css already exists to avoid everytime generation
doc: html/style.css

html/style.css:
	test -d html || mkdir html
	$(OCAMLDOC) $(MODULE_INCLUDES) $(P4FLAGS) -d html -html \
	-I $(top_srcdir)/src $(top_srcdir)/src/*.mli $(top_srcdir)/src/*.ml

# default clean
clean-am:
	rm -fR html/*.html html/*.css
