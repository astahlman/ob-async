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
