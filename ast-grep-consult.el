;;; ast-grep-consult.el --- Consult backend for ast-grep.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Consult async backend for ast-grep.el.  This file intentionally does
;; not require consult at load time; availability is checked when the
;; backend is selected.

;;; Code:

(require 'ast-grep-core)

(declare-function consult--async-pipeline "consult")
(declare-function consult--async-process "consult")
(declare-function consult--async-transform "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--read "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--file-preview "consult")

(defun ast-grep--consult-available-p ()
  "Return non-nil when consult is loadable."
  (require 'consult nil t))

(defun ast-grep--state ()
  "State function for ast-grep consult integration."
  (let ((preview (consult--file-preview)))
    (lambda (action cand)
      (if-let ((match (and cand (ast-grep--candidate-match cand))))
          (let ((file (plist-get match :file))
                (line (plist-get match :start-line))
                (column (plist-get match :start-column)))
            (pcase action
              ('preview
               (when-let ((buffer (funcall preview action file)))
                 (with-current-buffer buffer
                   (ast-grep--goto-line-column line column)
                   (pulse-momentary-highlight-one-line (point))
                   (point))))
              (_ (funcall preview action file))))
        (funcall preview action cand)))))

(defun ast-grep--async-builder (input directory)
  "Build ast-grep command for INPUT in DIRECTORY, or nil if INPUT is too short."
  (when (and input (>= (length input) ast-grep-async-min-input))
    (ast-grep--build-command input directory)))

(defun ast-grep--async-source (directory)
  "Create async source for DIRECTORY using consult."
  (consult--async-pipeline
   (consult--async-throttle)
   (consult--async-process
    (lambda (input) (ast-grep--async-builder input directory)))
   (consult--async-transform
    (lambda (items)
      (delq nil (mapcar #'ast-grep--parse-stream-line items))))))

(defun ast-grep--search-consult (directory)
  "Search asynchronously using consult in DIRECTORY.
The ast-grep pattern is typed in the minibuffer.  Results stream
in as you type; type after `#' to narrow with `completing-read'."
  (ast-grep--reset-candidate-table)
  (let ((source (ast-grep--async-source directory)))
    (when-let ((selection
                (consult--read
                 source
                 :prompt "ast-grep: "
                 :lookup #'consult--lookup-member
                 :state (ast-grep--state)
                 :annotate (lambda (cand)
                             (list cand
                                   (ast-grep--candidate-icon-prefix cand)
                                   ""))
                 :category 'ast-grep
                 :history 'ast-grep-history
                 :require-match t)))
      (ast-grep--goto-match selection))))

(provide 'ast-grep-consult)

;;; ast-grep-consult.el ends here
