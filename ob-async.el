;;; ob-async.el --- Asynchronous org-babel src block execution

;; Copyright (C) 2017 Andrew Stahlman

;; Author: Andrew Stahlman <andrewstahlman@gmail.com>
;; Created: 10 Feb 2017
;; Version: 0.1

;; Keywords: tools
;; Homepage: https://github.com/astahlman/ob-async

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((async "1.9") (org "9.0.1") (emacs "24.4") (dash "2.14.1"))

;;; Commentary:
;; This file enables asynchronous execution of org-babel
;; src blocks through the ob-async-org-babel-execute-src-block function

;;; Code:

(provide 'ob-async)

(require 'org)
(require 'async)
(require 'dash)

(defvar ob-async-no-async-languages-alist nil
  "async is not used for languages listed here. Enables
compatibility for other languages, e.g. ipython, for which async
functionality may be implemented separately.")

(defvar ob-async-pre-execute-src-block-hook nil
  "Hook run in the async child process prior to executing a src
block. You can use this hook to perform language-specific
initialization which would normally execute in your init file.")

(defvar ob-async-inject-variables "\\borg-babel.+"
  "Regex of variables that should be injected into the async process.
It's a good idea to include any variables that are prefixed with `org-babel'.
Add additional variables like \"\\(\\borg-babel.+\\|sql-connection-alist\\)\".")

;;;###autoload
(defalias 'org-babel-execute-src-block:async 'ob-async-org-babel-execute-src-block)

;;;###autoload
(defun ob-async-org-babel-execute-src-block (&optional orig-fun arg info params)
  "Like org-babel-execute-src-block, but run asynchronously.

Original docstring for org-babel-execute-src-block:

Execute the current source code block.  Insert the results of
execution into the buffer.  Source code execution and the
collection and formatting of results can be controlled through a
variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive "P")
  (cond
   ;; If this function is not called as advice, do nothing
   ((not orig-fun)
    (warn "ob-async-org-babel-execute-src-block is no longer needed in org-ctrl-c-ctrl-c-hook")
    nil)
   ;; If there is no :async parameter, call the original function
   ((not (assoc :async (nth 2 (or info (org-babel-get-src-block-info)))))
    (funcall orig-fun arg info params))
   ;; If the src block language is in the list of languages async is not to be
   ;; used for, call the original function
   ((member (nth 0 (or info (org-babel-get-src-block-info)))
            ob-async-no-async-languages-alist)
    (funcall orig-fun arg info params))
   ;; Otherwise, perform asynchronous execution
   (t
    (let ((placeholder (ob-async--generate-uuid)))
      ;; Here begins the original source of org-babel-execute-src-block
      (let* ((org-babel-current-src-block-location
              (or org-babel-current-src-block-location
                  (nth 5 info)
                  (org-babel-where-is-src-block-head)))
             (src-block-marker (save-excursion
                                 (goto-char org-babel-current-src-block-location)
                                 (point-marker)))
             (info (if info (copy-tree info) (org-babel-get-src-block-info))))
        ;; Merge PARAMS with INFO before considering source block
        ;; evaluation since both could disagree.
        (cl-callf org-babel-merge-params (nth 2 info) params)
        (when (org-babel-check-evaluate info)
          (cl-callf org-babel-process-params (nth 2 info))
          (let* ((params (nth 2 info))
                 (cache (let ((c (cdr (assq :cache params))))
                          (and (not arg) c (string= "yes" c))))
                 (new-hash (and cache (org-babel-sha1-hash info)))
                 (old-hash (and cache (org-babel-current-result-hash)))
                 (current-cache (and new-hash (equal new-hash old-hash)))
                 (result-params (cdr (assq :result-params params))))
            (cond
             (current-cache
              (save-excursion		;Return cached result.
                (goto-char (org-babel-where-is-src-block-result nil info))
                (forward-line)
                (skip-chars-forward " \t")
                (let ((result (org-babel-read-result)))
                  (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
                  result)))
             ((org-babel-confirm-evaluate info)
              ;; Insert a GUID as a placeholder in our RESULTS block
              (when (not (or (member "none" result-params)
                             (member "silent" result-params)))
                (org-babel-insert-result placeholder '("replace")))
              (let* ((lang (nth 0 info))
                     ;; Expand noweb references in BODY and remove any
                     ;; coderef.
                     (body
                      (let ((coderef (nth 6 info))
                            (expand
                             (if (org-babel-noweb-p params :eval)
                                 (org-babel-expand-noweb-references info)
                               (nth 1 info))))
                        (if (not coderef) expand
                          (replace-regexp-in-string
                           (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                     (dir (cdr (assq :dir params)))
                     (default-directory
                       (or (and dir (file-name-as-directory (expand-file-name dir)))
                           default-directory))
                     (cmd (intern (concat "org-babel-execute:" lang)))
                     (org-babel-async-content
                      (buffer-substring-no-properties (point-min) (point-max)))
                     result)
                (unless (fboundp cmd)
                  (error "No org-babel-execute function for %s!" lang))
                (message "executing %s code block%s..."
                         (capitalize lang)
                         (let ((name (nth 4 info)))
                           (if name (format " (%s)" name) "")))
                (progn
                  (async-start
                   `(lambda ()
                      ;; TODO: Put this in a function so it can be overidden
                      ;; Initialize the new Emacs process with org-babel functions
                      (setq exec-path ',exec-path)
                      (setq load-path ',load-path)
                      ,(async-inject-variables ob-async-inject-variables)
                      (package-initialize)
                      (setq ob-async-pre-execute-src-block-hook ',ob-async-pre-execute-src-block-hook)
                      (run-hooks 'ob-async-pre-execute-src-block-hook)
                      (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
                      (let ((default-directory ,default-directory))
                        (with-temp-buffer
                          (insert org-babel-async-content)
                          (,cmd ,body ',params))))
                   `(lambda (result)
                      (with-current-buffer ,(current-buffer)
                        (let ((default-directory ,default-directory))
                          (save-excursion
                            (cond
                              ((member "none" ',result-params)
                               (message "result silenced"))
                              ((member "silent" ',result-params)
                               (message (replace-regexp-in-string "%" "%%" (format "%S" result))))
                              (t
                               (goto-char ,src-block-marker)
                               (let ((file (cdr (assq :file ',params))))
                                 (when file
                                   ;; when result type is link, don't write result content to file.
                                   (unless (member "link" ',result-params)
                                     ;; If non-empty result and :file then write to :file.
                                     (when result
                                       (with-temp-file file
                                         (insert (org-babel-format-result
                                                  result (cdr (assq :sep ',params)))))))
                                   (setq result file))
                                 ;; Possibly perform post process provided its
                                 ;; appropriate.  Dynamically bind "*this*" to the
                                 ;; actual results of the block.
                                 (let ((post (cdr (assq :post ',params))))
                                   (when post
                                     (let ((*this* (if (not file) result
                                                     (org-babel-result-to-file
                                                      file
                                                      (let ((desc (assq :file-desc ',params)))
                                                        (and desc (or (cdr desc) result)))))))
                                       (setq result (org-babel-ref-resolve post))
                                       (when file
                                         (setq result-params (remove "file" ',result-params))))))
                                 (org-babel-insert-result result ',result-params ',info ',new-hash ',lang))))
                            (run-hooks 'org-babel-after-execute-hook)))))))))))))))))

(defun ob-async--generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (random 100000000))))

(advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block)

;;; ob-async.el ends here
