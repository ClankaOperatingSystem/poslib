# make check: lint, then test.
# make check-ipfs IPFS=path/to/ipfs: CID fixtures against kubo, offline.

EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L lisp
SRC    = lisp/pos.el lisp/pos-capture.el lisp/pos-cid.el lisp/pos-ledger.el
IPFS  ?= ipfs

.PHONY: check check-ipfs test lint clean

check: lint test

test:
	$(BATCH) -l ert -l test/pos-test.el -l test/pos-capture-test.el \
	         -l test/pos-cid-test.el -l test/pos-ledger-test.el \
	         -f ert-run-tests-batch-and-exit

check-ipfs:
	IPFS=$(IPFS) $(BATCH) -l test/pos-cid-ipfs.el -f pos-cid-ipfs-check

lint:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SRC); \
	 status=$$?; rm -f lisp/*.elc; exit $$status
	@out=$$($(BATCH) -l checkdoc --eval '(mapc (function checkdoc-file) (list "lisp/pos.el" "lisp/pos-capture.el" "lisp/pos-cid.el" "lisp/pos-ledger.el"))' 2>&1); \
	 echo "$$out"; ! echo "$$out" | grep -q '^Warning'

clean:
	rm -f lisp/*.elc
