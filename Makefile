SHELL := /bin/bash
-include local.mk
EMACS ?= $(shell which emacs)

check_local_mk:
	if [ ! -f "local.mk" ]; then echo "WARNING: No 'local.mk', will use $(EMACS) to run tests. You can override by creating a file named 'local.mk'"; fi
clone_or_pull_orgmode:
	if [ -d "org-mode" ]; then \
		cd org-mode && git pull; \
	else \
		git clone git://orgmode.org/org-mode.git; \
	fi
	cd org-mode && make autoloads EMACS=$(EMACS)
setup_test: check_local_mk clone_or_pull_orgmode
	mkdir -p elpa
test: setup_test
	$(EMACS) --script "./run-tests.el" -Q
