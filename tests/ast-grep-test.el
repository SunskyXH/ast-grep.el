;;; ast-grep-test.el --- Tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for ast-grep.el using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'ast-grep)

;;; Utility function tests

(ert-deftest ast-grep-test-executable-available-p ()
  "Test executable availability check."
  (let ((ast-grep-executable "nonexistent-command"))
    (should-not (ast-grep--executable-available-p)))
  (let ((ast-grep-executable "echo"))
    (should (ast-grep--executable-available-p))))

(ert-deftest ast-grep-test-build-command ()
  "Test command building."
  (let ((ast-grep-executable "ast-grep")
        (ast-grep-arguments '("--json")))
    (should (equal (ast-grep--build-command "pattern")
                   '("ast-grep" "run" "--pattern=pattern" "--json")))
    (should (equal (ast-grep--build-command "pattern" "/path")
                   '("ast-grep" "run" "--pattern=pattern" "--json" "/path")))))

(ert-deftest ast-grep-test-parse-json-output ()
  "Test JSON output parsing."
  (let ((json-output "[{\"file\":\"test.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"console.log()\"}]"))
    (should (equal (ast-grep--parse-json-output json-output)
                   '("test.js:1:0:console.log()"))))
  (should-not (ast-grep--parse-json-output ""))
  (should-not (ast-grep--parse-json-output nil)))

(ert-deftest ast-grep-test-goto-match ()
  "Test goto match functionality."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (let ((match "buffer:2:2:line 2"))
      (should (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" match))
      (should (equal (match-string 2 match) "2"))
      (should (equal (match-string 3 match) "2")))))

;;; Integration tests (require mock)

(ert-deftest ast-grep-test-candidates-empty-pattern ()
  "Test candidates function with empty pattern."
  (should-not (ast-grep--candidates ""))
  (should-not (ast-grep--candidates nil)))

(ert-deftest ast-grep-test-candidates-with-mock ()
  "Test candidates function with mocked ast-grep command."
  (cl-letf (((symbol-function 'ast-grep--run-command)
             (lambda (&rest args)
               "[{\"file\":\"test.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"console.log()\"}]")))
    (let ((candidates (ast-grep--candidates "console.log")))
      (should (equal candidates '("test.js:1:0:console.log()"))))))

(ert-deftest ast-grep-test-candidates-with-error ()
  "Test candidates function with command error."
  (cl-letf (((symbol-function 'ast-grep--run-command)
             (lambda (&rest args)
               (error "Command failed"))))
    (let ((candidates (ast-grep--candidates "pattern")))
      (should-not candidates))))

(provide 'test-ast-grep)

;;; ast-grep-test.el ends here