#!/bin/bash
(defun load-org-mode ()
  (add-to-list 'load-path "./org-mode/lisp")
  ;; Note: Org uses lower version when org-mode/contrib/lisp is on the load path
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (shell . t)))
  (setq org-confirm-babel-evaluate nil)
  (message "Running tests against org-version: %s" (org-version)))

(defun load-async ()
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (setq package-user-dir "/home/astahlman/workplace/org-babel-async/elpa")
  (require 'package)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'async)
  (require 'async)
  (message "Running tests against async-version: %s" (package-desc-version (cadr (assq 'async package-alist)))))

(load-org-mode)
(load-async)
(load "/home/astahlman/workplace/org-babel-async/org-babel-async.el")
(ert-run-tests-batch-and-exit)
