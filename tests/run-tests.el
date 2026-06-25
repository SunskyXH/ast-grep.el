;;; run-tests.el --- Load ast-grep.el test suites -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unified ERT entry point.  Set AST_GREP_TEST_BACKEND to sync,
;; consult, ivy, or full to choose the backend sandbox suite.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'ast-grep-test-helper)

(defconst ast-grep-test--all-test-files
  '("ast-grep-core-test.el"
    "ast-grep-outline-test.el"
    "ast-grep-sync-test.el"
    "ast-grep-consult-test.el"
    "ast-grep-ivy-test.el"
    "ast-grep-entry-test.el")
  "All test files loadable by `ast-grep-test-run-batch'.")

(defun ast-grep-test--test-files ()
  "Return the test files for the requested backend sandbox."
  (if (getenv "AST_GREP_TEST_SELECTOR")
      ast-grep-test--all-test-files
    (pcase (getenv "AST_GREP_TEST_BACKEND")
      ("sync" '("ast-grep-core-test.el"
                "ast-grep-outline-test.el"
                "ast-grep-sync-test.el"
                "ast-grep-entry-test.el"))
      ("consult" '("ast-grep-core-test.el"
                   "ast-grep-outline-test.el"
                   "ast-grep-consult-test.el"
                   "ast-grep-entry-test.el"))
      ("ivy" '("ast-grep-core-test.el"
               "ast-grep-outline-test.el"
               "ast-grep-ivy-test.el"
               "ast-grep-entry-test.el"))
      ("full" ast-grep-test--all-test-files)
      (_ ast-grep-test--all-test-files))))

(dolist (file (ast-grep-test--test-files))
  (load (expand-file-name file (file-name-directory
                               (or load-file-name buffer-file-name)))
        nil t))

(defun ast-grep-test--selector ()
  "Return the ERT selector for the requested backend sandbox."
  (or (getenv "AST_GREP_TEST_SELECTOR")
      (pcase (getenv "AST_GREP_TEST_BACKEND")
        ("sync" '(or "^ast-grep-core-test-"
                    "^ast-grep-outline-test-"
                    "^ast-grep-sync-test-"
                    "^ast-grep-entry-test-"))
        ("consult" '(or "^ast-grep-core-test-"
                       "^ast-grep-outline-test-"
                       "^ast-grep-consult-test-"
                       "^ast-grep-entry-test-"))
        ("ivy" '(or "^ast-grep-core-test-"
                   "^ast-grep-outline-test-"
                   "^ast-grep-ivy-test-"
                   "^ast-grep-entry-test-"))
        ("full" t)
        (_ t))))

(defun ast-grep-test-run-batch ()
  "Run ast-grep.el tests in batch mode."
  (ert-run-tests-batch-and-exit (ast-grep-test--selector)))

(provide 'ast-grep-run-tests)

;;; run-tests.el ends here
