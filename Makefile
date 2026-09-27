# make check: lint, then test.

EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L lisp
SRC    = lisp/pos.el lisp/pos-capture.el

.PHONY: check test lint clean

check: lint test

test:
	$(BATCH) -l ert -l test/pos-test.el -l test/pos-capture-test.el \
	         -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SRC); \
	 status=$$?; rm -f lisp/*.elc; exit $$status
	@out=$$($(BATCH) -l checkdoc --eval '(mapc (function checkdoc-file) (list "lisp/pos.el" "lisp/pos-capture.el"))' 2>&1); \
	 echo "$$out"; ! echo "$$out" | grep -q '^Warning'

clean:
	rm -f lisp/*.elc
