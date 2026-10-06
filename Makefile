# make check: lint, then test.
# make check-ipfs IPFS=path/to/ipfs: CID fixtures against kubo, offline.

EMACS ?= emacs
# Dependencies, from Package-Requires, each fetched from its Git repository
# at one commit into _deps/: markdown-mode 2.8 and yaml 1.2.4.
MARKDOWN_MODE = f5d520b3ee7722dd2231ab586ba51d8eb166e49b
YAML          = 5546f36bde24a9a8c1934e0f6ce205cd41d72537
BATCH  = $(EMACS) -Q --batch -L _deps/markdown-mode -L _deps/yaml -L lisp
SRC    = lisp/pos.el lisp/pos-capture.el lisp/pos-cid.el lisp/pos-ledger.el lisp/pos-links.el lisp/pos-seal.el lisp/pos-index.el lisp/pos-migrate.el lisp/pos-signin.el lisp/pos-remote.el lisp/pos-tree.el lisp/pos-startup.el
IPFS  ?= ipfs

.PHONY: check check-ipfs test lint clean deps

check: lint test

# fetch NAME URL COMMIT FILE: _deps/NAME at COMMIT with FILE compiled,
# fetched again if it is at another commit.
define fetch
	@if [ "$$(cat _deps/$(1)/.commit 2>/dev/null)" != "$(3)" ]; then \
	     rm -rf _deps/$(1) && git init -q _deps/$(1) \
	  && git -C _deps/$(1) fetch -q --depth 1 $(2) $(3) \
	  && git -C _deps/$(1) checkout -q FETCH_HEAD \
	  && $(EMACS) -Q --batch -f batch-byte-compile _deps/$(1)/$(4) 2>/dev/null \
	  && echo $(3) > _deps/$(1)/.commit; \
	 fi
endef

deps:
	$(call fetch,markdown-mode,https://github.com/jrblevin/markdown-mode.git,$(MARKDOWN_MODE),markdown-mode.el)
	$(call fetch,yaml,https://github.com/zkry/yaml.el.git,$(YAML),yaml.el)

test: deps
	$(BATCH) -l ert -l test/pos-test.el -l test/pos-capture-test.el \
	         -l test/pos-cid-test.el -l test/pos-ledger-test.el -l test/pos-seal-test.el -l test/pos-index-test.el -l test/pos-migrate-test.el -l test/pos-remote-test.el -l test/pos-signin-test.el -l test/pos-tree-test.el -l test/pos-startup-test.el \
	         -f ert-run-tests-batch-and-exit

check-ipfs: deps
	IPFS=$(IPFS) $(BATCH) -l test/pos-cid-ipfs.el -f pos-cid-ipfs-check

lint: deps
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SRC); \
	 status=$$?; rm -f lisp/*.elc; exit $$status
	@out=$$($(BATCH) -l checkdoc --eval '(mapc (function checkdoc-file) (list "lisp/pos.el" "lisp/pos-capture.el" "lisp/pos-cid.el" "lisp/pos-ledger.el" "lisp/pos-links.el" "lisp/pos-seal.el" "lisp/pos-index.el" "lisp/pos-migrate.el" "lisp/pos-signin.el" "lisp/pos-remote.el" "lisp/pos-tree.el" "lisp/pos-startup.el"))' 2>&1); \
	 echo "$$out"; ! echo "$$out" | grep -q '^Warning'

clean:
	rm -f lisp/*.elc
