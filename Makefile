setup:
	cd org-mode && git pull && make autoloads
test:
	./run-tests
publish:
	emacs --batch --file ob-async.org --eval "(org-babel-tangle)"
