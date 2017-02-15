setup:
	cd org-mode && git pull && make autoloads
	mkdir -p elpa
test: setup
	./run-tests
