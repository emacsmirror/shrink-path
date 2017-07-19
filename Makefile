emacs ?= emacs
CASK ?= cask
BEMACS = $(CASK) exec $(emacs) --batch -Q

cask:
	$(CASK) --verbose --debug

build:
	$(CASK) build

test:
	$(CASK) exec ert-runner --reporter ert --verbose

checkdoc:
	$(BEMACS) -l test/test-checkdoc.el

clean:
	$(CASK) clean-elc

.PHONY: cask test checkdoc clean
