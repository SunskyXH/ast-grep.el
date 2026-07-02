;;; ast-grep-helm.el --- Helm backend for ast-grep.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Helm async backend for ast-grep.el.  This file intentionally does
;; not require helm at load time; availability is checked when the
;; backend is selected.

;;; Code:

(require 'ast-grep-core)

(declare-function helm "helm")
(declare-function helm-make-source "helm-source")
(declare-function pulse-momentary-highlight-one-line "pulse")

(defvar helm-pattern)

(defun ast-grep--helm-ensure-function (function)
  "Ensure Helm FUNCTION is defined, loading Helm if needed."
  (unless (fboundp function)
    (require 'helm))
  (unless (fboundp function)
    (error "Helm function `%s' is not available" function)))

(defun ast-grep--helm-available-p ()
  "Return non-nil when helm is loadable."
  (and (require 'helm nil t)
       (fboundp 'helm)
       (fboundp 'helm-make-source)))

(defun ast-grep--helm-command (input directory)
  "Build ast-grep command for INPUT in DIRECTORY, or nil if INPUT is too short.
INPUT is measured with `string-width', not `length', because Helm gates
`:requires-pattern' by width; the two checks must agree or a wide-char
\(e.g. CJK) pattern passes Helm's gate, reaches
`ast-grep--helm-candidates-process', and errors instead of searching."
  (when (and input (>= (string-width input) ast-grep-async-min-input))
    (ast-grep--build-command input directory)))

(defun ast-grep--helm-candidates-process (directory)
  "Start an ast-grep process for the current `helm-pattern' in DIRECTORY."
  (let ((command (ast-grep--helm-command helm-pattern directory)))
    (unless command
      (error "Helm pattern is shorter than `ast-grep-async-min-input'"))
    (ast-grep--reset-candidate-table)
    (let ((process-connection-type nil)
          (coding-system-for-read 'utf-8))
      (apply #'start-process "ast-grep-helm" nil command))))

(defun ast-grep--helm-display-candidate (candidate)
  "Return a Helm display/real pair for CANDIDATE."
  (cons (concat (ast-grep--candidate-icon-prefix candidate) candidate)
        candidate))

(defun ast-grep--helm-filter-one-by-one (line)
  "Parse one JSON LINE from Helm async output into a display candidate."
  (when-let ((candidate (ast-grep--parse-stream-line line)))
    (ast-grep--helm-display-candidate candidate)))

(defun ast-grep--helm-action (candidate)
  "Visit CANDIDATE for the Helm backend."
  (when (ast-grep--candidate-match candidate)
    (ast-grep--goto-match candidate)
    (pulse-momentary-highlight-one-line (point))))

(defvar ast-grep--helm-preview-buffers nil
  "Buffers opened only to preview candidates in the current Helm session.")

(defun ast-grep--helm-preview (candidate)
  "Preview CANDIDATE for the Helm backend.
A buffer this opens is recorded in `ast-grep--helm-preview-buffers' so
`ast-grep--helm-cleanup' can kill it when the session ends; buffers
that were already live are left alone.  Selecting a candidate goes
through `ast-grep--helm-action' instead, which visits for good."
  (when-let* ((match (ast-grep--candidate-match candidate))
              (file (plist-get match :file)))
    (unless (find-buffer-visiting file)
      (push (find-file-noselect file) ast-grep--helm-preview-buffers))
    (ast-grep--goto-match candidate)
    (pulse-momentary-highlight-one-line (point))))

(defun ast-grep--helm-cleanup ()
  "Kill unmodified buffers opened only for preview during the Helm session."
  (dolist (buffer ast-grep--helm-preview-buffers)
    (when (and (buffer-live-p buffer)
               (not (buffer-modified-p buffer)))
      (kill-buffer buffer)))
  (setq ast-grep--helm-preview-buffers nil))

(defun ast-grep--helm-source (directory)
  "Create a Helm async source for DIRECTORY.
The source enables `helm-follow-mode' (`:follow' 1), so moving between
candidates previews them live; the cleanup hook kills buffers that were
opened purely for preview once the session ends."
  (ast-grep--helm-ensure-function 'helm-make-source)
  (helm-make-source
   "ast-grep" 'helm-source-async
   :candidates-process (lambda ()
                         (ast-grep--helm-candidates-process directory))
   :filter-one-by-one #'ast-grep--helm-filter-one-by-one
   :action #'ast-grep--helm-action
   :persistent-action #'ast-grep--helm-preview
   :persistent-help "Preview match"
   :cleanup #'ast-grep--helm-cleanup
   :follow 1
   :requires-pattern ast-grep-async-min-input
   :nohighlight t
   :nomark t))

(defun ast-grep--search-helm (directory)
  "Search asynchronously using Helm in DIRECTORY.
The ast-grep pattern is typed in the Helm minibuffer.  Results stream
in as you type after `ast-grep-async-min-input' characters, and the
selected candidate is previewed live (helm-follow-mode; toggle with
\\`C-c C-f').  Preview-only buffers are cleaned up when the session
ends."
  (ast-grep--helm-ensure-function 'helm)
  (ast-grep--reset-candidate-table)
  (helm :sources (ast-grep--helm-source directory)
        :prompt "ast-grep: "
        :buffer "*helm ast-grep*"
        :history 'ast-grep-history))

(provide 'ast-grep-helm)

;;; ast-grep-helm.el ends here
