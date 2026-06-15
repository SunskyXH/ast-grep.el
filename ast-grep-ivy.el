;;; ast-grep-ivy.el --- Ivy/counsel backend for ast-grep.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Counsel/ivy async backend for ast-grep.el.  This file intentionally
;; does not require ivy/counsel at load time; availability is checked when
;; the backend is selected.

;;; Code:

(require 'ast-grep-core)

(declare-function ivy-read "ivy")
(declare-function counsel--async-command "counsel")
(declare-function counsel--async-filter "counsel")
(declare-function counsel-delete-process "counsel")
(declare-function pulse-momentary-highlight-one-line "pulse")
(defvar counsel--async-timer nil
  "Timer used internally by counsel's async command debounce.")

(defconst ast-grep--ivy-process-name " *counsel*"
  "Default process buffer name used by counsel async backends.")

(defvar ast-grep--ivy-generation 0
  "Monotonic generation counter for the active ivy search session.")

(defun ast-grep--ivy-available-p ()
  "Return non-nil when both ivy and counsel are loadable."
  (and (require 'ivy nil t) (require 'counsel nil t)))

(defun ast-grep--command-shell-string (command)
  "Return COMMAND as a safely quoted shell command string."
  (ast-grep--command-string command))

(defun ast-grep--ivy-more-chars (input)
  "Return ivy placeholder candidates when INPUT is too short."
  (let ((remaining (- ast-grep-async-min-input (length input))))
    (when (> remaining 0)
      (list "" (format "%d chars more" remaining)))))

(defun ast-grep--ivy-next-generation ()
  "Advance and return the active ivy search generation."
  (setq ast-grep--ivy-generation (1+ ast-grep--ivy-generation)))

(defun ast-grep--ivy-cancel-pending-command ()
  "Cancel counsel's pending async command timer, when present."
  (when (and (boundp 'counsel--async-timer)
             (timerp counsel--async-timer))
    (cancel-timer counsel--async-timer)
    (setq counsel--async-timer nil)))

(defun ast-grep--ivy-stop-process ()
  "Stop any pending or running counsel process for ast-grep."
  (ast-grep--ivy-cancel-pending-command)
  (counsel-delete-process))

(defun ast-grep--ivy-current-process-p (process generation)
  "Return non-nil if PROCESS still belongs to ivy search GENERATION."
  (and (= generation ast-grep--ivy-generation)
       (eq process (get-process ast-grep--ivy-process-name))))

(defun ast-grep--ivy-async-filter (process raw &optional generation)
  "Filter PROCESS output RAW for the counsel/ivy async path.
Partial JSON lines are buffered per process.  Complete JSON lines are
parsed into structured matches and registered in
`ast-grep--candidate-table'; only display strings are forwarded to
counsel.
When GENERATION is non-nil, output from stale processes is ignored."
  (when (or (null generation)
            (ast-grep--ivy-current-process-p process generation))
    (let* ((pending (or (process-get process 'ast-grep--pending) ""))
           (chunk (concat pending raw))
           (parts (split-string chunk "\n"))
           (tail (car (last parts)))
           (complete (butlast parts))
           (lines (delq nil (mapcar #'ast-grep--parse-stream-line complete))))
      (process-put process 'ast-grep--pending tail)
      (when lines
        (counsel--async-filter
         process
         ;; Keep text properties so nerd-icons `display' prefixes survive.
         (concat (mapconcat #'identity lines "\n") "\n"))))))

(defun ast-grep--ivy-collection (directory)
  "Return a dynamic ivy collection function scoped to DIRECTORY."
  (lambda (input)
    (let ((generation (ast-grep--ivy-next-generation)))
      (if (or (null input) (< (length input) ast-grep-async-min-input))
          (progn
            (ast-grep--reset-candidate-table)
            (ast-grep--ivy-stop-process)
            (ast-grep--ivy-more-chars (or input "")))
        (ast-grep--reset-candidate-table)
        (ast-grep--ivy-stop-process)
        (counsel--async-command
         (ast-grep--command-shell-string
          (ast-grep--build-command input directory))
         nil
         (lambda (process raw)
           (ast-grep--ivy-async-filter process raw generation)))
        nil))))

(defun ast-grep--ivy-action (candidate)
  "Preview or visit CANDIDATE for the counsel/ivy backend."
  (when (ast-grep--candidate-match candidate)
    (ast-grep--goto-match candidate)
    (pulse-momentary-highlight-one-line (point))))

(defun ast-grep--search-ivy (directory)
  "Search asynchronously using counsel/ivy in DIRECTORY.
The ivy backend is isolated from consult because ivy owns minibuffer
state differently.  The async filter registers structured match data in
`ast-grep--candidate-table' before forwarding plain display strings to
counsel.  Candidate actions first recover data from that registry, then
fall back to the shared legacy display-string parser when registry
context is unavailable."
  (ast-grep--reset-candidate-table)
  (ivy-read "ast-grep: "
            (ast-grep--ivy-collection directory)
            :dynamic-collection t
            :action #'ast-grep--ivy-action
            :update-fn 'auto
            :unwind (lambda ()
                      (ast-grep--ivy-next-generation)
                      (ast-grep--ivy-stop-process))
            :history 'ast-grep-history
            :require-match t
            :caller 'ast-grep-search))

(provide 'ast-grep-ivy)

;;; ast-grep-ivy.el ends here
