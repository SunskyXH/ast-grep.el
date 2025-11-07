;;; test-helper.el --- Shared test helpers for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared helper functions and macros for ast-grep.el test suites.

;;; Code:

(defmacro ast-grep-with-executable-check (&rest body)
  "Execute BODY if ast-grep is available, otherwise skip test."
  `(progn
     (skip-unless (executable-find "ast-grep"))
     ,@body))

(provide 'test-helper)

;;; test-helper.el ends here
