install:
	make patch ; \
	for t in `find $(CILHOME) -name "*.sh"` ; do \
	 sudo chmod 774 $$t ; \
	done

uninstall:
	make patch PATCH_FLAGS=-R

patch:
	@if [ -z "$(CILHOME)" ]; then \
	 echo "CILHOME variable is not set."; \
	 exit; \
	fi; \
        CIL_PATCH_VERSION="`grep CIL_VERSION_MAJOR= $(CILHOME)/configure | awk -F= '{print $$2}'`.`grep CIL_VERSION_MINOR= $(CILHOME)/configure | awk -F= '{print $$2}'`.`grep CIL_VERSION_REV= $(CILHOME)/configure | awk -F= '{print $$2}'`";\
	if [ -d "$$CIL_PATCH_VERSION" ]; then \
	 echo "CIL version is: $$CIL_PATCH_VERSION." ;\
        else \
         echo "Patch for CIL version $$CIL_PATCH_VERSION not found." ;\
         exit ;\
	fi; \
	PATCH="`pwd`/$$CIL_PATCH_VERSION/diff.log"; \
	PATCH_FLAGS="$$PATCH_FLAGS -p1" ; \
	patch $$PATCH_FLAGS \
              -d $(CILHOME) \
              -i $$PATCH


