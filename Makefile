emacs ?= emacs
BEMACS = $(emacs) -batch -l test/elpa.el

all: emacsq

update:
	emacs -batch -l test/make-update.el

emacsq:
	emacs -Q -l test/elpa.el -l org-download.el ~/test.org

compile:
	$(BEMACS) -batch $(LOAD) --eval "(byte-compile-file \"org-download.el\")"

clean:
	rm -f *.elc

.PHONY: all update emacsq compile clean
