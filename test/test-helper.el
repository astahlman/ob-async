(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t)))
(setq org-confirm-babel-evaluate nil)
(message "org-version: %s" (org-version))
(add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)
