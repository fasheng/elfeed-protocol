EMACS ?= emacs
CASK ?= cask

.PHONY: install build test clean
all: install build test clean

# init cask environment
init:
	EMACS=$(EMACS) $(CASK) install

build:
	EMACS=$(EMACS) $(CASK) build

test:
	EMACS=$(EMACS) $(CASK) exec ert-runner

clean:
	EMACS=$(EMACS) $(CASK) clean-elc

checkdoc:
	$(CASK) exec $(EMACS) --batch -L . -l test/test-checkdoc.el -Q 2>&1 | tee __checkdoc
	@cat __checkdoc | [ $$(wc -l) -gt 0 ] && exit 1 || exit 0

elint:
	$(CASK) exec $(EMACS) --batch -L . --eval="(elint-directory \".\")"

package-lint:
	$(CASK) exec $(EMACS) --batch -L . -l elfeed.el -l package-lint.el -f package-lint-batch-and-exit elfeed-protocol.el
