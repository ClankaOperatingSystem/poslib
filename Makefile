# make check: lint, then test. Every lisp/*.el and test/*-test.el is
# taken up; a new file needs no entry here.
# make check-ipfs IPFS=path/to/ipfs: CID fixtures against kubo, offline.

EMACS ?= emacs
# Dependencies, from Package-Requires, each fetched from its Git repository
# at one commit into _deps/: markdown-mode 2.8, yaml 1.2.4, and org-roam
# 2.3.1 with what it requires: compat 31.1.0.0, cond-let 1.1.5, llama
# 1.0.6, dash 2.20.0, emacsql 4.4.2, magit-section 4.7.1, from magit, and
# transient 0.13.8, which magit-section wants newer than Emacs ships.
MARKDOWN_MODE = f5d520b3ee7722dd2231ab586ba51d8eb166e49b
YAML          = 5546f36bde24a9a8c1934e0f6ce205cd41d72537
COMPAT        = 90880f81419577e1d3f68424d2a3adf31e6d663e
COND_LET      = 09292a77001434f59ab55c775dec2b98cb18d028
LLAMA         = 6850d0c91b629da14fdff2300c222289d1a0029a
DASH          = b96413794b2fa9e37a17ca0d6fe0d0396006d3ec
EMACSQL       = 7a4c607912c8fdd1fca4def4915d68f43b12d4da
MAGIT         = 659f89955cf60fe3d4326d881c412df06c69680d
TRANSIENT     = 0cacc84ff0c7df126e194666ff8b8a1e6082e796
ORG_ROAM      = 7ce95a286ba7d0383f2ab16ca4cdbf79901921ff
DEPS   = -L _deps/markdown-mode -L _deps/yaml -L _deps/compat -L _deps/cond-let \
         -L _deps/llama -L _deps/dash -L _deps/emacsql -L _deps/transient/lisp \
         -L _deps/magit/lisp -L _deps/org-roam
BATCH  = $(EMACS) -Q --batch $(DEPS) -L lisp -L test
SRC    = $(wildcard lisp/*.el)
TESTS  = $(wildcard test/*-test.el)
SUPPORT = test/pos-test-support.el test/pos-fixtures.el
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
	  && $(EMACS) -Q --batch $(DEPS) -f batch-byte-compile _deps/$(1)/$(4) 2>/dev/null \
	  && echo $(3) > _deps/$(1)/.commit; \
	 fi
endef

deps:
	$(call fetch,markdown-mode,https://github.com/jrblevin/markdown-mode.git,$(MARKDOWN_MODE),markdown-mode.el)
	$(call fetch,yaml,https://github.com/zkry/yaml.el.git,$(YAML),yaml.el)
	$(call fetch,compat,https://github.com/emacs-compat/compat.git,$(COMPAT),compat.el)
	$(call fetch,cond-let,https://github.com/tarsius/cond-let.git,$(COND_LET),cond-let.el)
	$(call fetch,llama,https://github.com/tarsius/llama.git,$(LLAMA),llama.el)
	$(call fetch,dash,https://github.com/magnars/dash.el.git,$(DASH),dash.el)
	$(call fetch,emacsql,https://github.com/magit/emacsql.git,$(EMACSQL),emacsql.el)
	$(call fetch,transient,https://github.com/magit/transient.git,$(TRANSIENT),lisp/transient.el)
	$(call fetch,magit,https://github.com/magit/magit.git,$(MAGIT),lisp/magit-section.el)
	$(call fetch,org-roam,https://github.com/org-roam/org-roam.git,$(ORG_ROAM),org-roam.el)

test: deps
	$(BATCH) -l ert $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

check-ipfs: deps
	IPFS=$(IPFS) $(BATCH) -l test/pos-cid-ipfs.el -f pos-cid-ipfs-check

lint: deps
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SRC); \
	 status=$$?; rm -f lisp/*.elc; exit $$status
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(SUPPORT) $(TESTS); \
	 status=$$?; rm -f test/*.elc; exit $$status
	@out=$$($(BATCH) -l checkdoc \
	         --eval '(mapc (function checkdoc-file) (list $(patsubst %,"%",$(SRC) $(SUPPORT) $(TESTS))))' 2>&1); \
	 echo "$$out"; ! echo "$$out" | grep -q '^Warning'

clean:
	rm -f lisp/*.elc test/*.elc
