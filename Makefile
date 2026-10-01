# make check: lint, then test.
# make check-ipfs IPFS=path/to/ipfs: CID fixtures against kubo, offline.

EMACS ?= emacs
# Dependencies, from Package-Requires, installed from GNU and NonGNU ELPA into _deps/.
PKGS   = --eval '(progn (require (quote package)) (setq package-user-dir (expand-file-name "_deps") package-gnupghome-dir (expand-file-name "_deps/gnupg") package-archives (quote (("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/nongnu/")))) (package-initialize))'
BATCH  = $(EMACS) -Q --batch $(PKGS) -L lisp
SRC    = lisp/pos.el lisp/pos-capture.el lisp/pos-cid.el lisp/pos-ledger.el lisp/pos-links.el lisp/pos-seal.el lisp/pos-index.el lisp/pos-migrate.el lisp/pos-remote.el lisp/pos-tree.el
IPFS  ?= ipfs

.PHONY: check check-ipfs test lint clean deps

check: lint test

deps:
	@$(BATCH) --eval '(dolist (pkg (quote (markdown-mode yaml))) (unless (package-installed-p pkg) (package-refresh-contents) (package-install pkg)))'

test: deps
	$(BATCH) -l ert -l test/pos-test.el -l test/pos-capture-test.el \
	         -l test/pos-cid-test.el -l test/pos-ledger-test.el -l test/pos-seal-test.el -l test/pos-index-test.el -l test/pos-migrate-test.el -l test/pos-remote-test.el -l test/pos-tree-test.el \
	         -f ert-run-tests-batch-and-exit

check-ipfs: deps
	IPFS=$(IPFS) $(BATCH) -l test/pos-cid-ipfs.el -f pos-cid-ipfs-check

lint: deps
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SRC); \
	 status=$$?; rm -f lisp/*.elc; exit $$status
	@out=$$($(BATCH) -l checkdoc --eval '(mapc (function checkdoc-file) (list "lisp/pos.el" "lisp/pos-capture.el" "lisp/pos-cid.el" "lisp/pos-ledger.el" "lisp/pos-links.el" "lisp/pos-seal.el" "lisp/pos-index.el" "lisp/pos-migrate.el" "lisp/pos-remote.el" "lisp/pos-tree.el"))' 2>&1); \
	 echo "$$out"; ! echo "$$out" | grep -q '^Warning'

clean:
	rm -f lisp/*.elc
