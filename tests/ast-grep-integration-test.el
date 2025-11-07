;;; ast-grep-integration-test.el --- Integration tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Integration tests for ast-grep.el that test real interactions with
;; the ast-grep executable and simulate user workflows.

;;; Code:

(require 'ert)
(require 'ast-grep)

;; Load shared test helpers
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helper" test-dir)))

;;; Test helpers

(defvar ast-grep-integration-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing integration tests.")

(defvar ast-grep-integration-fixtures-dir
  (expand-file-name "fixtures" ast-grep-integration-test-dir)
  "Directory containing test fixtures.")

;;; Integration tests

(ert-deftest ast-grep-integration-test-executable-check ()
  "Test that ast-grep executable is available for integration tests."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (should (ast-grep--executable-available-p)))

(ert-deftest ast-grep-integration-test-search-console-log ()
  "Integration test: Search for console.log in fixtures."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (ast-grep-with-executable-check
   (let* ((default-directory ast-grep-integration-fixtures-dir)
          (results (ast-grep--candidates "console.log($_)")))
     (should results)
     (should (> (length results) 0))
     ;; Should find console.log in sample.js
     (should (seq-some (lambda (r) (string-match-p "sample\\.js" r)) results)))))

(ert-deftest ast-grep-integration-test-search-function-def ()
  "Integration test: Search for function definitions in fixtures."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-integration-fixtures-dir)
         (results (ast-grep--candidates "function $NAME($$$)")))
    (should results)
    (should (> (length results) 0))
    ;; Should find functions in sample.js
    (should (seq-some (lambda (r) (string-match-p "greet\\|farewell" r)) results))))

(ert-deftest ast-grep-integration-test-search-class ()
  "Integration test: Search for class definitions in fixtures."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-integration-fixtures-dir)
         (results (ast-grep--candidates "class $NAME { $$$ }")))
    (should results)
    (should (> (length results) 0))
    ;; Should find Person class
    (should (seq-some (lambda (r) (string-match-p "Person" r)) results))))

(ert-deftest ast-grep-integration-test-parse-result-format ()
  "Integration test: Verify result format is parseable."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-integration-fixtures-dir)
         (results (ast-grep--candidates "console.log($_)")))
    (should results)
    (dolist (result results)
      ;; Each result should match the format: file:line:column:text
      (should (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" result))
      (should (match-string 1 result))  ; file
      (should (match-string 2 result))  ; line
      (should (match-string 3 result))))) ; column

(ert-deftest ast-grep-integration-test-empty-pattern ()
  "Integration test: Empty pattern should return nil."
  (let ((results (ast-grep--candidates "")))
    (should-not results)))

(ert-deftest ast-grep-integration-test-nonexistent-pattern ()
  "Integration test: Search for pattern that doesn't exist."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-integration-fixtures-dir)
         (results (ast-grep--candidates "nonExistentFunction($_)")))
    ;; Should return empty list for pattern that doesn't match anything
    (should-not results)))

(ert-deftest ast-grep-integration-test-build-command ()
  "Integration test: Verify command building."
  (let ((cmd (ast-grep--build-command "pattern")))
    (should (equal cmd '("ast-grep" "run" "--pattern=pattern" "--json=stream"))))
  (let ((cmd (ast-grep--build-command "pattern" "/tmp")))
    (should (equal cmd '("ast-grep" "run" "--pattern=pattern" "--json=stream" "/tmp")))))

(ert-deftest ast-grep-integration-test-stream-parsing ()
  "Integration test: Verify streaming JSON output parsing."
  (let ((stream-line (concat "{"
                             "\"file\":\"test.js\","
                             "\"range\":{\"start\":{\"line\":5,\"column\":2}},"
                             "\"text\":\"console.log('test')\""
                             "}")))
    (let ((result (ast-grep--parse-stream-line stream-line)))
      (should result)
      (should (string-match "^test\\.js:6:2:" result)))))

(ert-deftest ast-grep-integration-test-multiple-results ()
  "Integration test: Search for pattern with multiple matches."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-integration-fixtures-dir)
         ;; Search for console.log which appears multiple times
         (results (ast-grep--candidates "console.log")))
    (when results
      ;; Should find multiple console.log statements
      (should (>= (length results) 2)))))

(provide 'ast-grep-integration-test)

;;; ast-grep-integration-test.el ends here
