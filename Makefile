setup:
	cd org-mode && git pull && make autoloads
test:
	./run-tests
