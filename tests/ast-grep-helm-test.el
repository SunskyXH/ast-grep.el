;;; ast-grep-helm-test.el --- Helm backend tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-helm)

(ert-deftest ast-grep-helm-test-command-respects-min-input ()
  "Builder returns nil when input is shorter than `ast-grep-async-min-input'."
  (let ((ast-grep-async-min-input 3)
        (ast-grep-executable "ast-grep"))
    (should-not (ast-grep--helm-command "" "/dir"))
    (should-not (ast-grep--helm-command "ab" "/dir"))
    (should (equal (ast-grep--helm-command "abc" "/dir")
                   '("ast-grep" "run" "--pattern=abc"
                     "--json=stream" "/dir")))))

(ert-deftest ast-grep-helm-test-candidates-process-starts-command ()
  "The Helm adapter starts ast-grep directly with argv, not a shell string."
  (let ((ast-grep-async-min-input 3)
        (ast-grep-executable "ast-grep")
        (helm-pattern "abc")
        (candidate (ast-grep--format-candidate
                    (list :file "stale.js"
                          :start-line 0
                          :start-column 0
                          :text "stale")))
        captured)
    (should (ast-grep--candidate-match candidate))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args)
                 (setq captured args)
                 'process)))
      (should (eq (ast-grep--helm-candidates-process "/dir") 'process))
      (should (equal captured
                     '("ast-grep-helm" nil "ast-grep" "run"
                       "--pattern=abc" "--json=stream" "/dir")))
      (should-not (gethash (substring-no-properties candidate)
                           ast-grep--candidate-table)))))

(ert-deftest ast-grep-helm-test-candidates-process-rejects-short-input ()
  "Calling the process builder below the min-input threshold is an error."
  (let ((ast-grep-async-min-input 3)
        (helm-pattern "ab"))
    (should-error (ast-grep--helm-candidates-process "/dir"))))

(ert-deftest ast-grep-helm-test-filter-one-by-one-parses-json ()
  "The Helm line transformer registers matches and returns display/real pairs."
  (let ((ast-grep-use-nerd-icons nil))
    (ast-grep--reset-candidate-table)
    (let ((candidate
           (ast-grep--helm-filter-one-by-one
            "{\"file\":\"a:b.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x:y\"}")))
      (should (consp candidate))
      (should (equal (substring-no-properties (car candidate))
                     "a:b.js:1:0:x:y"))
      (should (equal (substring-no-properties (cdr candidate))
                     "a:b.js:1:0:x:y"))
      (let ((match (ast-grep--candidate-match (cdr candidate))))
        (should (equal (plist-get match :file) "a:b.js"))
        (should (= (plist-get match :start-line) 0))
        (should (= (plist-get match :start-column) 0))
        (should (equal (plist-get match :text) "x:y"))))))

(ert-deftest ast-grep-helm-test-filter-one-by-one-ignores-invalid-json ()
  "Malformed process lines are dropped."
  (should-not (ast-grep--helm-filter-one-by-one "not json")))

(ert-deftest ast-grep-helm-test-source-wires-helm-slots ()
  "The Helm source uses async process slots and shared candidate actions."
  (let (captured proc-directory)
    (cl-letf (((symbol-function 'helm-make-source)
               (lambda (&rest args)
                 (setq captured args)
                 args))
              ((symbol-function 'ast-grep--helm-candidates-process)
               (lambda (directory)
                 (setq proc-directory directory)
                 'process)))
      (let* ((source (ast-grep--helm-source "/dir"))
             (slots (cddr source))
             (candidate-process (plist-get slots :candidates-process)))
        (should (equal (car source) "ast-grep"))
        (should (eq (cadr source) 'helm-source-async))
        (should (functionp candidate-process))
        (should (eq (funcall candidate-process) 'process))
        (should (equal proc-directory "/dir"))
        (should (eq (plist-get slots :filter-one-by-one)
                    #'ast-grep--helm-filter-one-by-one))
        (should (eq (plist-get slots :action) #'ast-grep--helm-action))
        (should (eq (plist-get slots :persistent-action)
                    #'ast-grep--helm-action))
        (should (eq (plist-get slots :requires-pattern)
                    ast-grep-async-min-input))))))

(ert-deftest ast-grep-helm-test-real-source-builds-in-backend-sandbox ()
  "The Helm source constructor works with the real Helm slot API."
  (skip-unless ast-grep-test--helm-available)
  (let ((source (ast-grep--helm-source ast-grep-test--fixtures-dir)))
    (should (equal (assoc-default 'name source) "ast-grep"))
    (should (eq (assoc-default 'requires-pattern source)
                ast-grep-async-min-input))
    (should (functionp (assoc-default 'candidates-process source)))))

(ert-deftest ast-grep-helm-test-search-wires-helm-call ()
  "`ast-grep--search-helm' creates the source and starts Helm."
  (let (helm-args source-directory)
    (cl-letf (((symbol-function 'ast-grep--helm-source)
               (lambda (directory)
                 (setq source-directory directory)
                 'source))
              ((symbol-function 'helm)
               (lambda (&rest args)
                 (setq helm-args args)
                 'helm-result)))
      (should (eq (ast-grep--search-helm ast-grep-test--fixtures-dir)
                  'helm-result))
      (should (equal source-directory ast-grep-test--fixtures-dir))
      (should (eq (plist-get helm-args :sources) 'source))
      (should (equal (plist-get helm-args :prompt) "ast-grep: "))
      (should (equal (plist-get helm-args :buffer) "*helm ast-grep*"))
      (should (eq (plist-get helm-args :history) 'ast-grep-history)))))

(ert-deftest ast-grep-helm-test-action-wires-goto-match ()
  "The Helm action visits the selected match and pulses the destination."
  (let* ((selection (ast-grep--format-candidate
                     (list :file ast-grep-test--sample-file
                           :start-line 4
                           :start-column 2
                           :text "console.log(name)")))
         (buffer-before (find-buffer-visiting ast-grep-test--sample-file))
         (pulse-calls 0))
    (cl-letf (((symbol-function 'pulse-momentary-highlight-one-line)
               (lambda (&rest _) (cl-incf pulse-calls))))
      (unwind-protect
          (progn
            (ast-grep--helm-action selection)
            (should (equal (buffer-file-name) ast-grep-test--sample-file))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2))
            (should (= pulse-calls 1)))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting ast-grep-test--sample-file)))
            (kill-buffer buf)))))))

(ert-deftest ast-grep-helm-test-action-ignores-non-candidates ()
  "The Helm action ignores placeholder text and unmatched strings."
  (let ((find-file-calls 0)
        (pulse-calls 0))
    (cl-letf (((symbol-function 'find-file)
               (lambda (&rest _) (cl-incf find-file-calls)))
              ((symbol-function 'pulse-momentary-highlight-one-line)
               (lambda (&rest _) (cl-incf pulse-calls))))
      (should-not (ast-grep--helm-action "not a candidate"))
      (should (zerop find-file-calls))
      (should (zerop pulse-calls)))))

(ert-deftest ast-grep-helm-test-helm-available-in-backend-sandbox ()
  "Helm backend sandboxes must load real Helm."
  (skip-unless (member (getenv "AST_GREP_TEST_BACKEND") '("helm" "full")))
  (should ast-grep-test--helm-available)
  (should (ast-grep--helm-available-p)))

(provide 'ast-grep-helm-test)

;;; ast-grep-helm-test.el ends here
