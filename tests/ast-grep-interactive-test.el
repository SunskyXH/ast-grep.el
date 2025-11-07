;;; ast-grep-interactive-test.el --- Interactive tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive tests that simulate user workflows with ast-grep.el,
;; including buffer navigation, command execution, and result handling.

;;; Code:

(require 'ert)
(require 'ast-grep)

;; Load shared test helpers
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helper" test-dir)))

;;; Test helpers

(defvar ast-grep-interactive-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing interactive tests.")

(defvar ast-grep-interactive-fixtures-dir
  (expand-file-name "fixtures" ast-grep-interactive-test-dir)
  "Directory containing test fixtures.")

;;; Interactive workflow tests

(ert-deftest ast-grep-interactive-test-goto-match ()
  "Test navigation to search result."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (ast-grep-with-executable-check
   (let* ((default-directory ast-grep-interactive-fixtures-dir)
          (sample-file (expand-file-name "sample.js" ast-grep-interactive-fixtures-dir))
          (results (ast-grep--candidates "console.log($_)")))
     (skip-unless results)
     (let ((first-result (car results)))
      ;; Parse the result
      (should (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" first-result))
      (let ((file (match-string 1 first-result))
            (line (string-to-number (match-string 2 first-result)))
            (column (string-to-number (match-string 3 first-result))))
        ;; Verify parsing
        (should file)
        (should (> line 0))
        (should (>= column 0))
        ;; Test navigation in a buffer
        (with-temp-buffer
          (insert-file-contents sample-file)
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column column)
          ;; Verify we're at a reasonable position
          (should (not (eobp)))
          (should (not (bobp))))))))

(ert-deftest ast-grep-interactive-test-buffer-state ()
  "Test that searches don't corrupt buffer state."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-interactive-fixtures-dir)
         (original-buffer (current-buffer))
         (original-point (point)))
    ;; Run a search
    (ast-grep--candidates "console.log($_)")
    ;; Verify buffer state is unchanged
    (should (eq (current-buffer) original-buffer))
    (should (= (point) original-point))))

(ert-deftest ast-grep-interactive-test-multiple-searches ()
  "Test running multiple searches in sequence."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let ((default-directory ast-grep-interactive-fixtures-dir))
    ;; First search
    (let ((results1 (ast-grep--candidates "console.log($_)")))
      (should results1))
    ;; Second search
    (let ((results2 (ast-grep--candidates "function $NAME($$$)")))
      (should results2))
    ;; Third search
    (let ((results3 (ast-grep--candidates "class $NAME { $$$ }")))
      (should results3))))

(ert-deftest ast-grep-interactive-test-result-text-extraction ()
  "Test that result text is properly extracted."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-interactive-fixtures-dir)
         (results (ast-grep--candidates "console.log($_)")))
    (skip-unless results)
    (dolist (result results)
      ;; Extract text portion (after third colon)
      (when (string-match "^[^:]+:[0-9]+:[0-9]+:\\(.*\\)" result)
        (let ((text (match-string 1 result)))
          (should text)
          (should (not (string-empty-p text)))
          ;; Text should contain console.log
          (should (string-match-p "console\\.log" text)))))))

(ert-deftest ast-grep-interactive-test-file-exists ()
  "Test that result files actually exist."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-interactive-fixtures-dir)
         (results (ast-grep--candidates "console.log($_)")))
    (skip-unless results)
    (dolist (result results)
      (when (string-match "^\\([^:]+\\):" result)
        (let ((file (match-string 1 result)))
          ;; File should exist
          (should (file-exists-p (expand-file-name file ast-grep-interactive-fixtures-dir))))))))

(ert-deftest ast-grep-interactive-test-search-with-directory ()
  "Test search with explicit directory parameter."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  ;; Change to a different directory
  (let ((default-directory "/tmp"))
    ;; Search should work with explicit directory
    (let ((results (ast-grep--candidates "console.log($_)" 
                                         ast-grep-interactive-fixtures-dir)))
      (should results)
      (should (> (length results) 0)))))

(ert-deftest ast-grep-interactive-test-error-handling ()
  "Test error handling with invalid pattern."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let ((default-directory ast-grep-interactive-fixtures-dir))
    ;; Empty pattern should return nil gracefully
    (should-not (ast-grep--candidates ""))
    ;; Nil pattern should return nil gracefully
    (should-not (ast-grep--candidates nil))))

(ert-deftest ast-grep-interactive-test-project-root ()
  "Test project root detection."
  (let ((default-directory ast-grep-interactive-fixtures-dir))
    ;; This might be nil if not in a project, which is acceptable
    (let ((root (ast-grep--project-root)))
      ;; Should either be nil or a valid directory
      (when root
        (should (file-directory-p root))))))

(ert-deftest ast-grep-interactive-test-mode-activation ()
  "Test that ast-grep-mode can be activated."
  (with-temp-buffer
    ;; Mode should activate without errors
    (should-not (ast-grep-mode 1))
    ;; And deactivate cleanly
    (should-not (ast-grep-mode -1))))

(ert-deftest ast-grep-interactive-test-command-output-format ()
  "Test that command output is properly formatted."
  :expected-result (if (executable-find "ast-grep") :passed :failed)
  (skip-unless (executable-find "ast-grep"))
  (let* ((default-directory ast-grep-interactive-fixtures-dir)
         (results (ast-grep--candidates "console.log($_)")))
    (skip-unless results)
    (dolist (result results)
      ;; Result should have the format: file:line:column:text
      (should (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)" result))
      ;; Line should be positive
      (should (> (string-to-number (match-string 2 result)) 0))
      ;; Column should be non-negative
      (should (>= (string-to-number (match-string 3 result)) 0)))))

(provide 'ast-grep-interactive-test)

;;; ast-grep-interactive-test.el ends here
