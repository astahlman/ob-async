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

;; Package-Requires: ((async "1.9") (org "9.0.1"))

;;; Commentary:
;; This file enables asynchronous execution of org-babel
;; src blocks through the org-babel-execute-src-block:async function

;;; Code:

(provide 'ob-async)

(defun placeholder-p (s)
  "Return non-nil if S is a placeholder for an asynchronous result."
  (and (= 32 (length s)) (string-match-p "^[a-z0-9]\\{32\\}$" s)))

(defun results-block-contents ()
  "Return the contents of the *only* results block in the buffer."
  (interactive)
  (save-excursion
    (progn
      (goto-char 0)
      (org-babel-next-src-block)
      (goto-char (org-babel-where-is-src-block-result))
      (let ((result (org-babel-read-result)))
        (message "RESULTS: %s" result)
        result))))

(defmacro with-buffer-contents (s &rest forms)
  "Create a temporary buffer with contents S and execute FORMS."
  `(save-excursion
     (with-temp-buffer
       (progn
	 (goto-char 0)
	 (insert ,s)
	 (goto-char 0)
	 ,@forms))))

(defun wait-for-seconds (n)
  "Sleep for N seconds.  This is a workaround for a bug in sleep-for.
See http://stackoverflow.com/questions/14698081/elisp-sleep-for-doesnt-block-when-running-a-test-in-ert"
  (let ((deadline (+ n (float-time))))
    (while (< (float-time) deadline)
      (sleep-for 1))))

(ert-deftest test-async-execute-fresh-sh-block ()
  "Test that we can insert results for a sh block that hasn't been executed yet"
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
      sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (string= "Sorry for the wait." (results-block-contents))))))

(ert-deftest test-async-execute-existing-sh-block ()
  "Test that we can insert results for a sh block that has already been executed"
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (string= "Sorry for the wait." (results-block-contents)))
			  (goto-char 0)
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (string= "Sorry for the wait." (results-block-contents))))))

(ert-deftest test-async-execute-python-block ()
  "Test that we can insert results for a sh block that hasn't been executed yet"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (= 2 (results-block-contents))))))

(ert-deftest test-async-return-to-point-above-block ()
  "Test that results are inserted in the correct location
when content has been added above the source block"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (re-search-backward "block:")
			  (end-of-line)
			  (newline-and-indent)
			  (insert "Here's some more stuff while we're waiting")
			  (let ((captured-point (point)))
			    (wait-for-seconds 5)
			    (should (= 2 (results-block-contents)))
			    (should (= captured-point (point)))
			    (should (re-search-backward "some more stuff"))
			    (should (re-search-forward "BEGIN_SRC python"))))))

(ert-deftest test-async-return-to-point-below-block ()
  "Test that results are inserted in the correct location
when content has been added below the source block"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (goto-char (point-max))
			  (newline-and-indent)
			  (insert "Here's some more stuff while we're waiting")
			  (let* ((captured-point (point))
				 (expected-point (- captured-point (- (length (ob-async--generate-uuid))
								      (length "2")))))
			    (wait-for-seconds 5)
			    (should (= 2 (results-block-contents)))
			    (should (= expected-point (point)))
			    (should (re-search-backward "some more stuff"))
			    (should (re-search-backward "END_SRC"))))))

(ert-deftest test-async-execute-file-block ()
  "Test that we can insert results when header-arg :file is present"
  (let ((buffer-contents "Here's a sh source block:

  #+BEGIN_SRC sh :async :file \"/tmp/foo\"
     echo \"Don't wait on me\"
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (string= "/tmp/foo" (results-block-contents)))
			  (let ((foo-contents (progn (find-file "/tmp/foo") (buffer-substring-no-properties (point-min) (point-max)))))
			    (should (string= "Don't wait on me\n" foo-contents))))))

(ert-deftest test-async-execute-table-output ()
  "Test that we can insert table output"
  (let ((buffer-contents "Here's a source block:

#+BEGIN_SRC python :results output table :async t
x = [['{},{}    '.format(i, j) for j in range(1, 3)] for i in range(1, 3)]
for row in x:
    print '{}\\n'.format(x)
#+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (message "Waiting")
			  (wait-for-seconds 8)
			  (should (equal '(("1,1" "1,2") ("2,1" "2,2")) (results-block-contents)))
			  (message "%s" (results-block-contents)))))

(ert-deftest test-async-execute-silent-block ()
  "Test that we can insert results for a sh block that hasn't been executed yet"
  :expected-result :failed
  (let ((buffer-contents "Here's a sh source block:

  #+BEGIN_SRC sh :async :results silent
  echo \"Don't wait on me\"
  #+END_SRC"))
    (with-buffer-contents buffer-contents
			  (org-babel-next-src-block)
			  (org-ctrl-c-ctrl-c)
			  (should (placeholder-p (results-block-contents)))
			  (wait-for-seconds 5)
			  (should (not (results-block-contents))))))

;;;###autoload
(defalias 'org-babel-execute-src-block:async 'ob-async-org-babel-execute-src-block)

;;;###autoload
(defun ob-async-org-babel-execute-src-block (&optional arg info params)
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
  (when (and (org-in-src-block-p)
             (assoc :async (nth 2 (org-babel-get-src-block-info))))
    (let ((placeholder (ob-async--generate-uuid)))
      (org-babel-insert-result placeholder '("replace"))
      ;; This is the original source of org-babel-execute-src-block
      (let* ((org-babel-current-src-block-location
	    (or org-babel-current-src-block-location
		(nth 5 info)
		(org-babel-where-is-src-block-head)))
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
	       (current-cache (and new-hash (equal new-hash old-hash))))
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
	    (let* ((lang (nth 0 info))
		   (result-params (cdr (assq :result-params params)))
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
		      (package-initialize)
		      (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
		      (,cmd ,body ',params))
                   (if (member "none" ',result-params)
		       (progn (message "result silenced")
			      'ignore)
                     `(lambda (result)
			(switch-to-buffer ,(current-buffer))
			(point-to-register 13) ;; TODO: totally arbitrary choice of register
			(goto-char (point-min))
			(re-search-forward ,placeholder)
			(org-babel-previous-src-block)
			(let ((file (cdr (assq :file ',params))))
                          ;; If non-empty result and :file then write to :file.
                          (when file
                            (when result
			      (with-temp-file file
				(insert (org-babel-format-result
					 result (cdr (assq :sep ',params))))))
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
                          (org-babel-insert-result result ',result-params ',info ',new-hash ',lang)
                          (run-hooks 'org-babel-after-execute-hook))
                          (goto-char (point-min))
                          (jump-to-register 13))))))))))))))

(defun ob-async--generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (random 100000000))))

;;; ob-async.el ends here
