;;; ast-grep-sync-test.el --- Sync backend tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-sync)

(ert-deftest ast-grep-sync-test-search-prompts-and-jumps ()
  "`ast-grep--search-sync' prompts up front and jumps to the selection."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((target ast-grep-test--sample-file)
         (selection (format "%s:5:2:console.log(name)" target))
         (read-string-calls 0)
         (buffer-before (find-buffer-visiting target)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 (cl-incf read-string-calls)
                 "console.log($A)"))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) selection)))
      (unwind-protect
          (progn
            (ast-grep--search-sync ast-grep-test--fixtures-dir)
            (should (= 1 read-string-calls))
            (should (equal (buffer-file-name) target))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2)))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting target)))
            (kill-buffer buf)))))))
(provide 'ast-grep-sync-test)

;;; ast-grep-sync-test.el ends here
