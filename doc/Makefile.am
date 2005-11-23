# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
#AM_P4FLAGS = -pp "$(CAMLP4R) $(PARSER_INCLUDES)"

# check if style.css already exists to avoid everytime generation
all-am: html/style.css

html/style.css:
	$(OCAMLDOC) -I $(top_srcdir)/src $(MODULE_INCLUDES) $(P4FLAGS) \
	-d html -html $(top_srcdir)/src/*.mli $(top_srcdir)/src/*.ml

# default clean
clean-am:
	rm -fR html/*.html html/*.css
