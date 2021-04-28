(require 'subr-x)
(require 'cl-macs)
(require 'cl-lib)

(defun placeholder-p (s)
  "Return non-nil if S is a placeholder for an asynchronous result."
  (and (= 32 (length s)) (string-match-p "^[a-z0-9]\\{32\\}$" s)))

(defun results-block-contents (&optional name)
  "Return the contents of the *only* results block in the buffer.
If NAME is non-nil, find the results block by name."
  (interactive)
  (save-excursion
    (progn
      (if name
          (org-babel-goto-named-result name)
        (-if-let ((result-pos (org-babel-where-is-src-block-result)))
            (goto-char result-pos)
          (cl-assert (progn
                       (goto-char (point-min))
                       (re-search-forward "#\\+RESULT" nil t))
                     nil
                     "Couldn't find a RESULTS block")))
      (let ((result (org-babel-read-result)))
        (message "RESULTS: %s" result)
        result))))

(defmacro with-buffer-contents (s &rest forms)
  "Create a temporary buffer with contents S and execute FORMS."
  (declare (indent 1) (debug t))
  `(save-excursion
     (with-temp-buffer
       (progn
         (goto-char 0)
         (insert ,s)
         (goto-char 0)
         ,@forms))))

(defmacro ctrl-c-ctrl-c-with-callbacks (pre-callback pre-form post-callback post-form)
  "Run `org-ctrl-c-ctrl-c' with the supplied :pre and :post callbacks.

PRE-FORM is executed immediately after running `org-ctrl-c-ctrl-c'
POST-FORM is executed after the src-block finishes execution"
  (declare (debug (":pre" form ":post" form)))
  (let ((finished (cl-gensym))
        (sleep-interval-seconds (cl-gensym))
        (timeout-seconds (cl-gensym))
        (max-attempts (cl-gensym))
        (num-attempts (cl-gensym)))
    `(let* ((,finished nil)
            (,sleep-interval-seconds .5)
            (,timeout-seconds 10)
            (,max-attempts (/ ,timeout-seconds ,sleep-interval-seconds))
            (,num-attempts 0)
            (org-babel-after-execute-hook (list (lambda ()
                                                  ,post-form
                                                  (setq ,finished t)))))
       (org-ctrl-c-ctrl-c)
       ,pre-form
       (while (and
               (not ,finished)
               (<= (cl-incf ,num-attempts) ,max-attempts))
         (sleep-for ,sleep-interval-seconds))
       (when (not ,finished)
         (ert-fail "Timed out waiting for src-block execution to complete")))))

(ert-deftest test-async-execute-fresh-sh-block ()
  "Test that we can insert results for a sh block that hasn't been executed yet"
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
      sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait." (results-block-contents)))))))


(ert-deftest test-async-ignore-lang-sh-block ()
  "Testing ignoring a language."
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
      sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC")
	(ob-async-no-async-languages-alist '("sh")))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (should (string= "Sorry for the wait." (results-block-contents))))))

(ert-deftest test-async-execute-existing-sh-block ()
  "Test that we can insert results for a sh block that has already been executed"
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait." (results-block-contents))))
      (goto-char 0)
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait." (results-block-contents)))))))

(ert-deftest test-async-execute-python-block ()
  "Test that we can insert results for a sh block that hasn't been executed yet"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (= 2 (results-block-contents)))))))

(ert-deftest test-async-return-to-point-above-block ()
  "Test that results are inserted in the correct location
when content has been added above the source block"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (let (captured-point done-with-callback)
        (ctrl-c-ctrl-c-with-callbacks
         :pre (progn
                (should (placeholder-p (results-block-contents)))
                (re-search-backward "block:")
                (end-of-line)
                (newline-and-indent)
                (insert "Here's some more stuff while we're waiting")
                (setq captured-point (point)))
         :post (progn
                 (should (= 2 (results-block-contents)))
                 (should (re-search-backward "some more stuff"))
                 (should (re-search-forward "BEGIN_SRC python"))
                 (setq done-with-callback t)))
        ;; Our :post callback runs inside the context of
        ;; `org-babel-execute-src-block:async'. We specifically want
        ;; to check where (point) is when all our callbacks are
        ;; finished, hence this sentinel
        (let* ((sleep-interval-seconds .05)
               (five-seconds 5)
               (deadline (+ five-seconds (float-time))))
          (while (and (not done-with-callback)
                      (< (float-time) deadline))
            (sleep-for sleep-interval-seconds)))
        (should (= captured-point (point)))))))

(ert-deftest test-async-return-to-point-below-block ()
  "Test that results are inserted in the correct location
when content has been added below the source block"
  (let ((buffer-contents "Here's a Python source block:

  #+BEGIN_SRC python :async
      return 1 + 1
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (let (captured-point done-with-callback)
        (ctrl-c-ctrl-c-with-callbacks
         :pre (progn
                (should (placeholder-p (results-block-contents)))
                (goto-char (point-max))
                (newline-and-indent)
                (insert "Here's some more stuff while we're waiting")
                (setq captured-point (point)))
         :post (progn
                 (should (= 2 (results-block-contents)))
                 (setq done-with-callback t)))
        ;; Our :post callback runs inside the context of
        ;; `org-babel-execute-src-block:async'. We specifically want
        ;; to check where (point) is when all our callbacks are
        ;; finished, hence this sentinel
        (let* ((sleep-interval-seconds .05)
               (five-seconds 5)
               (deadline (+ five-seconds (float-time))))
          (while (and (not done-with-callback)
                      (< (float-time) deadline))
            (sleep-for sleep-interval-seconds)))
        (let ((expected-point (- captured-point (- (length (ob-async--generate-uuid))
                                                   (length "2")))))
          (should (= expected-point (point)))
          (should (re-search-backward "some more stuff"))
          (should (re-search-backward "END_SRC")))))))

(ert-deftest test-async-execute-file-block ()
  "Test that we can insert results when header-arg :file is present"
  (let ((buffer-contents "Here's a sh source block:

  #+BEGIN_SRC sh :async :results output file :file \"/tmp/test-async-execute-file-block\"
     echo \"Don't wait on me\"
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (progn
               (should (string= "/tmp/test-async-execute-file-block" (results-block-contents)))
               (let ((foo-contents (progn
                                     (find-file "/tmp/test-async-execute-file-block")
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
                 (should (string= "Don't wait on me\n" foo-contents))))))))

(ert-deftest test-async-execute-file-link-result-block ()
  "Test that we can insert results when header-arg :file and :results link is present."
  (let ((buffer-contents "Here's a sh source block:

  #+BEGIN_SRC sh :async :results link :file \"/tmp/test-async-execute-file-link-result-block\"
     echo \"Don't wait on me\" > /tmp/test-async-execute-file-link-result-block
  #+END_SRC"))
    (with-buffer-contents
     buffer-contents
     (org-babel-next-src-block)
     (ctrl-c-ctrl-c-with-callbacks
      :pre (should (placeholder-p (results-block-contents)))
      :post (progn
              (should (string= "/tmp/test-async-execute-file-link-result-block" (results-block-contents)))
              (let ((file-contents (progn
                                     (let ((revert-without-query '(".*")))
                                       (find-file "/tmp/test-async-execute-file-link-result-block"))
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
                (should (string= "Don't wait on me\n" file-contents))))))))

;; org-table parsing is flaky on Emacs 24.5
(unless (string-match "GNU Emacs 24[.]" (emacs-version))
  (ert-deftest test-async-execute-table-output ()
    "Test that we can insert table output"
    (let ((buffer-contents "Here's a source block:

#+BEGIN_SRC python :results output table :async t
x = [['{},{}    '.format(i, j) for j in range(1, 3)] for i in range(1, 3)]
for row in x:
    print('{}\\n'.format(x))
#+END_SRC"))
      (with-buffer-contents buffer-contents
                            (org-babel-next-src-block)
                            (ctrl-c-ctrl-c-with-callbacks
                             :pre (should (placeholder-p (results-block-contents)))
                             :post (should (equal '(("1,1" "1,2") ("2,1" "2,2"))
                                                  (results-block-contents))))))))

(ert-deftest test-async-execute-tramp-block ()
  "Test that we can execute a block via Tramp with a :dir header-arg"
  (let ((buffer-contents (format "Here's a sh source block:

  #+BEGIN_SRC sh :async :dir \"/sudo:%s@localhost:/\"
     echo $SUDO_USER $PWD
  #+END_SRC" user-login-name)))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= (format "%s /" user-login-name)
                              (results-block-contents)))))))

(ert-deftest test-async-ctrl-c-ctrl-c-hook ()
  "Test that asynchronous execution works with ctrl-c-ctrl-c-hook."
  (let ((buffer-contents "Here's a shell source block:

  #+BEGIN_SRC sh :async
      sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC")
        (org-ctrl-c-ctrl-c-hook '(ob-async-org-babel-execute-src-block)))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait."
                              (results-block-contents)))))))

(ert-deftest test-async-execute-named-block ()
  "Test that we can asynchronously execute a block when cursor is on the name."
  (let ((buffer-contents "Here's a shell source block:
  #+NAME: async block with spaces in the name
  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (re-search-forward "#\\+NAME")
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait."
                              (results-block-contents)))))))

(ert-deftest test-async-execute-named-block-with-results ()
  "Test that we can asynchronously execute a named block when results are anywhere in buffer."
  (let ((buffer-contents "Here's a shell source block:
  #+RESULTS: async block with spaces in the name

  #+NAME: async block with spaces in the name
  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (re-search-forward "#\\+NAME")
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (progn
               (should (string= "Sorry for the wait."
                                (results-block-contents)))
               (let ((results-block-pos (save-excursion
                                          (goto-char (point-min))
                                          (re-search-forward "#\\+RESULTS")
                                          (point-marker)))
                     (src-block-pos (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "#\\+BEGIN_SRC")
                                      (point-marker))))
                 (should (< results-block-pos src-block-pos))))))))

(ert-deftest test-async-execute-named-call-block ()
  "Test that we can asynchronously execute a named call block ."
  (let ((buffer-contents "
  #+NAME: test
  #+CALL: async-block()

  Here's a shell source block:

  #+NAME: async-block
  #+BEGIN_SRC sh :async
     echo 'Sorry for the wait.'
  #+END_SRC "))
    (with-buffer-contents buffer-contents
      (re-search-forward "#\\+NAME: test")
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents "test")))
       :post (let ((results-block-pos (save-excursion
                                        (org-babel-goto-named-result "test")
                                        (point-marker)))
                   (src-block-pos (save-excursion
                                    (org-babel-goto-named-src-block "async-block")
                                    (point-marker))))
               (should (< results-block-pos src-block-pos))
               (goto-char results-block-pos)
               (should (string= "Sorry for the wait."
                                (results-block-contents "test"))))))))

(ert-deftest test-async-results-none ()
  "Test that `:results none' blocks should have no output"
  (let ((buffer-contents "
  #+BEGIN_SRC sh :async :results none
  echo 'The lost message.'
  #+END_SRC"))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (string-equal buffer-contents (buffer-string)))
       :post (should (string-equal buffer-contents (buffer-string)))))))

(ert-deftest test-async-execute-call ()
  "Test that we can asynchronously execute a #+CALL element."
  (let ((buffer-contents "Here's a shell source block:
  #+NAME: async-block
  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC

  #+CALL: async-block()"))
    (with-buffer-contents buffer-contents
      (re-search-forward "#\\+CALL")
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (string= "Sorry for the wait."
                       (results-block-contents)))))))

(ert-deftest test-confirm-evaluate ()
  "Test that we do not add a RESULTS block if evaluation is not confirmed"
  (let ((buffer-contents "
  #+BEGIN_SRC sh :async
     sleep 1s && echo 'Sorry for the wait.'
  #+END_SRC")
        (org-confirm-babel-evaluate t)
        (org-babel-confirm-evaluate-answer-no t))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (should (not (org-babel-where-is-src-block-result))))))

(ert-deftest test-pre-execute-hook ()
  "Test that we can use a hook to perform setup before async execution"
  (let ((buffer-contents "
  #+BEGIN_SRC emacs-lisp :async
     (funcall this-function-is-defined-in-a-hook 1 1)
  #+END_SRC")
        (ob-async-pre-execute-src-block-hook '((lambda ()
                                                 (setq this-function-is-defined-in-a-hook #'+)))))
    (with-buffer-contents buffer-contents
      (org-babel-next-src-block)
      (ctrl-c-ctrl-c-with-callbacks
       :pre (should (placeholder-p (results-block-contents)))
       :post (should (= 2 (results-block-contents)))))))

(ert-deftest test-output-to-file-with-dir ()
  "Test that :file paths are resolved referenced relative to :dir"
  (let* ((output-file "ob-async--test-output-to-file-with-dir.txt")
         (uuid (ob-async--generate-uuid))
         (buffer-contents (format "
#+BEGIN_SRC sh :dir /tmp :file %s :async
  echo %s
#+END_SRC" output-file uuid)))
    (unwind-protect
        (progn
          (with-buffer-contents buffer-contents
            (org-babel-next-src-block)
            (ctrl-c-ctrl-c-with-callbacks
             :pre (should (placeholder-p (results-block-contents)))
             :post (let ((retrieved-value (with-temp-buffer
                                            (insert-file-contents (expand-file-name output-file "/tmp"))
                                            (string-trim (buffer-string)))))
                     (should (string= uuid retrieved-value))))))
      ;; Clean up after ourselves
      (when (file-exists-p output-file)
        (delete-file output-file)))))

(ert-deftest test-org-babel-vars-are-set-in-subprocess ()
  "Test that any variables prefixed with \"org-babel-\" are
inherited by the async subprocess"
  (let* ((org-babel-some-custom-variable "I should be set!")
         (uuid (ob-async--generate-uuid))
         (buffer-contents "
#+BEGIN_SRC emacs-lisp :async
  org-babel-some-custom-variable
#+END_SRC"))
    (unwind-protect
        (progn
          (with-buffer-contents buffer-contents
            (org-babel-next-src-block)
            (ctrl-c-ctrl-c-with-callbacks
             :pre (should (placeholder-p (results-block-contents)))
             :post (should (string= "I should be set!" (results-block-contents)))))))))

(ert-deftest test-inject-variables-are-set-in-subprocess ()
  "Test that variables in the `ob-sync-inject-variables' regex are
inherited by the async subprocess"
  (let* ((ob-async-inject-variables "\\(\\borg-babel.+\\|some-injected-variable\\)")
         (some-injected-variable "I should be set!")
         (uuid (ob-async--generate-uuid))
         (buffer-contents "
#+BEGIN_SRC emacs-lisp :async
  some-injected-variable
#+END_SRC"))
    (unwind-protect
        (progn
          (with-buffer-contents buffer-contents
            (org-babel-next-src-block)
            (ctrl-c-ctrl-c-with-callbacks
             :pre (should (placeholder-p (results-block-contents)))
             :post (should (string= "I should be set!" (results-block-contents)))))))))

(ert-deftest test-async-execute-with-ob-ref ()
  "Test that async subprocess works properly with ob-ref"
  (let* ((uuid (ob-async--generate-uuid))
         (buffer-contents "
#+NAME: result
#+begin_example
success
#+end_example

#+BEGIN_SRC emacs-lisp :async
(org-babel-ref-resolve \"result\")
#+END_SRC"))
    (unwind-protect
        (progn
          (with-buffer-contents buffer-contents
                                (org-babel-next-src-block)
                                (ctrl-c-ctrl-c-with-callbacks
                                 :pre (should (placeholder-p (results-block-contents)))
                                 :post (should (string= "success" (results-block-contents)))))))))
