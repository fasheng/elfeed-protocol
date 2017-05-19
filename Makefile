EMACS ?= emacs
CASK ?= cask
CASKEMACS = $(CASK) exec $(EMACS)

.PHONY: install build test clean
all: install build test clean

# install dependencies
install:
	EMACS=${EMACS} $(CASK) install

build:
	EMACS=${EMACS} $(CASK) build

test:
	EMACS=${EMACS} $(CASK) exec ert-runner

clean:
	EMACS=${EMACS} $(CASK) clean-elc
