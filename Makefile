TRAVIS_FILE=.travis.yml

.PHONY : build test-travis test clean

build :
	cask build

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis lint $(TRAVIS_FILE); fi

install:
	cask install

install-dev:
	@echo "Using emacs from $(shell which emacs)"
	EMACS=$(shell which emacs) cask install --dev

test :
	cask exec ert-runner --verbose --debug -l ob-async.el

clean :
	@rm -f *.elc *~ */*.elc */*~
