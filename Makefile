.POSIX:
EMACS   = emacs
BATCH   = $(EMACS) -batch -Q -L . -L tests
VERSION = 2.1.1

EL   = elfeed-backends.el elfeed-backends-ocnews.el
TEST = tests/elfeed-tests.el tests/elfeed-ocnews-tests.el

compile: $(EL:.el=.elc) $(TEST:.el=.elc)

test: $(EL:.el=.elc) $(TEST:.el=.elc)
	$(BATCH) -l tests/elfeed-backends-tests.elc -f ert-run-tests-batch

clean:
	rm -f $(EL:.el=.elc) $(TEST:.el=.elc)

.SUFFIXES: .el .elc

.el.elc:
	$(BATCH) -f batch-byte-compile $<
