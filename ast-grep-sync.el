;;; ast-grep-sync.el --- Synchronous backend for ast-grep.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Synchronous completing-read backend for ast-grep.el.

;;; Code:

(require 'ast-grep-core)

(defun ast-grep--search-sync (directory)
  "Search synchronously using `completing-read' in DIRECTORY."
  (ast-grep--reset-candidate-table)
  (let* ((pattern (read-string "ast-grep pattern: " nil 'ast-grep-history))
         (output (ast-grep--run-command pattern directory))
         (candidates (ast-grep--parse-stream-output output)))
    (if candidates
        (when-let ((selection
                    (completing-read
                     (format "ast-grep [%s]: " pattern)
                     candidates nil t nil 'ast-grep-history)))
          (ast-grep--goto-match selection))
      (progn
        (message "No matches found for pattern: %s" pattern)
        nil))))

(provide 'ast-grep-sync)

;;; ast-grep-sync.el ends here
