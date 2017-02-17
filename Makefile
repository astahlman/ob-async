SHELL := /bin/bash
-include local.mk
EMACS ?= $(shell which emacs)

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch

WORK_DIR=$(shell pwd)
PACKAGE_NAME=$(shell basename $(WORK_DIR))
TRAVIS_FILE=.travis.yml
TEST_DIR=ert-tests

.PHONY : build test-travis test clean 

check_local_mk:
	if [ ! -f "local.mk" ]; then echo "WARNING: No 'local.mk', will use $(EMACS) to run tests. You can override by creating a file named 'local.mk'"; fi

build :
	cask build

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis lint $(TRAVIS_FILE); fi

install:
	cask install

install-dev:
	cask install --dev

test :
	cask exec ert-runner --verbose --debug -l ob-async.el

clean :
	@rm -f *.elc *~ */*.elc */*~
