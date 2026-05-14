;;; ast-grep.el --- Search code using ast-grep with completing-read interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, matching
;; URL: https://github.com/sunskyxh/ast-grep.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs interface to ast-grep, a CLI tool for
;; code structural search, lint and rewriting.  It integrates with the
;; completing-read interface (Vertico, etc.) to provide fast
;; and interactive code searching based on Abstract Syntax Tree patterns.

;; Features:
;; - Search code using ast-grep patterns
;; - Project-wide search
;; - Integration with completing-read (Vertico, etc.)
;; - Streaming JSON parsing for efficient processing
;; - Async search with live results (when consult is available)

;;; Code:

(require 'project)

;; Declare external functions to avoid byte-compiler warnings
(declare-function consult--async-pipeline "consult")
(declare-function consult--async-process "consult")
(declare-function consult--async-transform "consult")
(declare-function consult--async-throttle "consult")
(declare-function consult--read "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--file-preview "consult")

(defgroup ast-grep nil
  "Search code using ast-grep."
  :group 'tools
  :prefix "ast-grep-")

(defcustom ast-grep-executable "ast-grep"
  "Path to the ast-grep executable."
  :type 'string
  :group 'ast-grep)


(defcustom ast-grep-debug nil
  "Enable debug output for ast-grep commands.
When non-nil, ast-grep will output debug information including
the command being executed, working directory, and raw output."
  :type 'boolean
  :group 'ast-grep)

(defcustom ast-grep-async-min-input 3
  "Minimum input length before triggering async search."
  :type 'integer
  :group 'ast-grep)

;;; Internal variables

(defvar ast-grep-history nil
  "History list for ast-grep searches.")

(defvar ast-grep-rewrite-history nil
  "History list for ast-grep rewrite templates.")

(defvar ast-grep--current-process nil
  "Current ast-grep process.")

;;; Utility functions

(defun ast-grep--executable-available-p ()
  "Check if ast-grep executable is available."
  (executable-find ast-grep-executable))

(defun ast-grep--project-root ()
  "Get the current project root directory."
  (when-let ((project (project-current)))
    (project-root project)))

(defun ast-grep--build-command (pattern &optional directory rewrite)
  "Build ast-grep command with PATTERN.
Optional DIRECTORY restricts the search path.  Optional REWRITE adds
a `--rewrite' template so ast-grep emits replacement text alongside
each match."
  (let ((cmd (list ast-grep-executable "run"))
        (args '()))
    (push (format "--pattern=%s" pattern) args)
    (when rewrite
      (push (format "--rewrite=%s" rewrite) args))
    (push "--json=stream" args)
    (when directory
      ;; Expand ~ and environment variables in directory path
      (push (expand-file-name directory) args))
    (append cmd (reverse args))))

;;; Core functions (kept for testing and internal use)

(defun ast-grep--run-command (pattern &optional directory)
  "Run ast-grep command with PATTERN in DIRECTORY."
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  
  (let ((default-directory (or directory default-directory))
        (command (ast-grep--build-command pattern directory)))
    ;; Debug output
    (when ast-grep-debug
      (message "Debug: Running command: %s" (mapconcat #'shell-quote-argument command " "))
      (message "Debug: Working directory: %s" default-directory))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process
                              (car command) nil t nil
                              (cdr command))))
        (let ((output (buffer-string)))
          (when ast-grep-debug
            (message "Debug: Command output: %s" output)
            (message "Debug: Exit code: %d" exit-code))
          (if (zerop exit-code)
              output
            (error "The ast-grep failed with exit code %d: %s"
                   exit-code output)))))))

(defun ast-grep--parse-json-output (output)
  "Parse ast-grep JSON OUTPUT into a list of candidates."
  (when (and output (not (string-empty-p output)))
    (let ((json-results (json-parse-string output :object-type 'plist)))
      (mapcar (lambda (result)
                (let* ((file (plist-get result :file))
                       (range (plist-get result :range))
                       (start (plist-get range :start))
                       (line (plist-get start :line))
                       (column (plist-get start :column))
                       (text (plist-get result :text)))
                  (format "%s:%d:%d:%s" file (1+ line) column (string-trim text))))
              json-results))))

(defun ast-grep--candidates (pattern &optional directory)
  "Get candidates for PATTERN in DIRECTORY."
  (when (and pattern (not (string-empty-p pattern)))
    (condition-case err
        (let ((output (ast-grep--run-command pattern directory)))
          (ast-grep--parse-json-output output))
      (error
       (message "ast-grep error: %s" (error-message-string err))
       nil))))

;;; Streaming functions

(defun ast-grep--parse-stream-line (line)
  "Parse a single JSON LINE from streaming output."
  (when (and line (not (string-empty-p line)))
    (condition-case nil
        (let* ((result (json-parse-string line :object-type 'plist))
               (file (plist-get result :file))
               (range (plist-get result :range))
               (start (plist-get range :start))
               (line-num (plist-get start :line))
               (column (plist-get start :column))
               (text (plist-get result :text)))
          (format "%s:%d:%d:%s" file (1+ line-num) column (string-trim text)))
      (error nil))))

(defun ast-grep--parse-stream-output (output)
  "Parse streaming JSON OUTPUT into a list of candidates."
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t)))
      (delq nil (mapcar #'ast-grep--parse-stream-line lines)))))

;;; Rewrite functions

(defun ast-grep--parse-rewrite-line (line)
  "Parse a single JSON LINE from ast-grep rewrite output.
Return a plist with :file, :start-line, :start-column, :end-line,
:end-column, :text, :replacement, or nil for malformed input."
  (when (and line (not (string-empty-p line)))
    (condition-case nil
        (let* ((result (json-parse-string line :object-type 'plist))
               (range (plist-get result :range))
               (start (plist-get range :start))
               (end (plist-get range :end)))
          (list :file (plist-get result :file)
                :start-line (plist-get start :line)
                :start-column (plist-get start :column)
                :end-line (plist-get end :line)
                :end-column (plist-get end :column)
                :text (plist-get result :text)
                :replacement (plist-get result :replacement)))
      (error nil))))

(defun ast-grep--collect-rewrites (pattern rewrite directory)
  "Run ast-grep with PATTERN and REWRITE in DIRECTORY.
Return a list of match plists as produced by
`ast-grep--parse-rewrite-line'."
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (let* ((default-directory (or directory default-directory))
         (command (ast-grep--build-command pattern directory rewrite))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'call-process
                                           (car command) nil t nil
                                           (cdr command))))
                     (when ast-grep-debug
                       (message "Debug: rewrite command: %s"
                                (mapconcat #'shell-quote-argument command " "))
                       (message "Debug: rewrite exit code: %d" exit-code))
                     (buffer-string)))))
    (delq nil (mapcar #'ast-grep--parse-rewrite-line
                      (split-string output "\n" t)))))

(defun ast-grep--match-region (match)
  "Compute (BEG . END) buffer positions in current buffer for MATCH.
Uses `forward-char' rather than `move-to-column' because ast-grep
reports character-index columns, while `move-to-column' uses visual
columns that change with `tab-width' and double-width characters."
  (save-excursion
    (goto-char (point-min))
    (forward-line (plist-get match :start-line))
    (forward-char (plist-get match :start-column))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (plist-get match :end-line))
      (forward-char (plist-get match :end-column))
      (cons beg (point)))))

(defun ast-grep--rewrite-sort (matches)
  "Return MATCHES sorted by file ascending, position descending.
Reversing within a file lets edits be applied without invalidating
the offsets of earlier matches in the same file."
  (sort (copy-sequence matches)
        (lambda (a b)
          (let ((fa (plist-get a :file))
                (fb (plist-get b :file)))
            (cond
             ((not (equal fa fb)) (string< fa fb))
             ((/= (plist-get a :start-line) (plist-get b :start-line))
              (> (plist-get a :start-line) (plist-get b :start-line)))
             (t (> (plist-get a :start-column)
                   (plist-get b :start-column))))))))

(defun ast-grep--apply-rewrites (matches)
  "Walk MATCHES interactively, applying replacements after confirmation.
The prompt accepts y (yes), n (skip), A or ! (apply this and all
remaining), q (quit).  Affected file-visiting buffers are saved
after the session completes; other modified buffers are untouched."
  (let ((replaced 0)
        (skipped 0)
        (modified-buffers nil)
        (all nil)
        (quit nil))
    (dolist (m (ast-grep--rewrite-sort matches))
      (unless quit
        (let* ((file (plist-get m :file))
               (buf (or (find-buffer-visiting file)
                        (find-file-noselect file))))
          (with-current-buffer buf
            (let* ((region (ast-grep--match-region m))
                   (beg (car region))
                   (end (cdr region))
                   (overlay (make-overlay beg end)))
              (overlay-put overlay 'face 'query-replace)
              (overlay-put overlay 'priority 1001)
              (unwind-protect
                  (progn
                    (pop-to-buffer buf)
                    (goto-char beg)
                    (let ((choice
                           (if all
                               ?y
                             (condition-case nil
                                 (read-char-choice
                                  (format "Replace `%s' with `%s'? (y/n/A/q) "
                                          (plist-get m :text)
                                          (plist-get m :replacement))
                                  '(?y ?n ?A ?a ?! ?q))
                               (quit ?q)))))
                      (pcase choice
                        ((or ?y ?A ?a ?!)
                         (when (memq choice '(?A ?a ?!)) (setq all t))
                         (goto-char beg)
                         (delete-region beg end)
                         (insert (plist-get m :replacement))
                         (unless (memq buf modified-buffers)
                           (push buf modified-buffers))
                         (setq replaced (1+ replaced)))
                        (?n (setq skipped (1+ skipped)))
                        (?q (setq quit t)))))
                (delete-overlay overlay)))))))
    ;; Save the buffers we edited.  Other modified buffers are left alone.
    (dolist (b modified-buffers)
      (when (and (buffer-live-p b)
                 (buffer-file-name b))
        (with-current-buffer b
          (when (buffer-modified-p)
            (save-buffer)))))
    (message "Replaced %d match(es) in %d file(s); skipped %d%s"
             replaced
             (length modified-buffers)
             skipped
             (if quit " (quit)" ""))))

;;; Async functions (require consult)

(defun ast-grep--state ()
  "State function for ast-grep consult integration."
  (let ((preview (consult--file-preview)))
    (lambda (action cand)
      (when cand
        (if (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" cand)
            (let ((file (match-string 1 cand))
                  (line (string-to-number (match-string 2 cand)))
                  (column (string-to-number (match-string 3 cand))))
              (pcase action
                ('preview
                 (when-let ((buffer (funcall preview action file)))
                   (with-current-buffer buffer
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (move-to-column column)
                     (pulse-momentary-highlight-one-line (point))
                     (point))))
                ('return
                 (find-file file)
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (move-to-column column))
                (_ (funcall preview action file))))
          (funcall preview action cand))))))

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

(defun ast-grep--search-async (directory)
  "Search asynchronously using consult in DIRECTORY.
The ast-grep pattern is typed in the minibuffer.  Results stream
in as you type; type after `#' to narrow with `completing-read'."
  (let ((source (ast-grep--async-source directory)))
    (consult--read
     source
     :prompt "ast-grep: "
     :lookup #'consult--lookup-member
     :state (ast-grep--state)
     :category 'ast-grep
     :history 'ast-grep-history
     :require-match t)))

(defun ast-grep--search-sync (pattern directory)
  "Search synchronously using `completing-read' for PATTERN in DIRECTORY."
  (let* ((output (ast-grep--run-command pattern directory))
         (candidates (ast-grep--parse-stream-output output)))
    (if candidates
        (completing-read
         (format "ast-grep [%s]: " pattern)
         candidates nil t nil 'ast-grep-history)
      (progn
        (message "No matches found for pattern: %s" pattern)
        nil))))

;;; Interactive commands

;;;###autoload
(defun ast-grep-search (&optional directory)
  "Search for ast-grep patterns in DIRECTORY.

DIRECTORY defaults to current directory.

When `consult' is available the pattern is typed directly in the
minibuffer and results stream in live.  Otherwise the pattern is
read first, then matches are shown via `completing-read'.

Example patterns:
  console.log($A)     - Find console.log calls
  $A && $A()          - Find conditional function calls
  function $NAME() {} - Find function declarations"
  (interactive)
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (let ((search-dir (or directory default-directory)))
    (when-let ((selection
                (if (require 'consult nil t)
                    (ast-grep--search-async search-dir)
                  (ast-grep--search-sync
                   (read-string "ast-grep pattern: " nil 'ast-grep-history)
                   search-dir))))
      (ast-grep--goto-match selection))))

;;;###autoload
(defun ast-grep-project ()
  "Search for ast-grep patterns in current project.

Requires being in a project (detected via `project.el').
Equivalent to `ast-grep-search' with the project root as directory."
  (interactive)
  (if-let ((project-root (ast-grep--project-root)))
      (ast-grep-search project-root)
    (error "Not in a project")))

;;;###autoload
(defun ast-grep-directory (directory)
  "Search for ast-grep patterns in DIRECTORY.

DIRECTORY supports `~' expansion."
  (interactive (list (read-directory-name "Directory: ")))
  (ast-grep-search directory))

;;;###autoload
(defun ast-grep-rewrite (&optional directory)
  "Search for ast-grep patterns in DIRECTORY and rewrite them interactively.

Prompts for a pattern and a replacement template, then walks each
match asking for confirmation, similar to
`project-query-replace-regexp'.  The prompt accepts y (yes), n
\(skip), A or ! (apply this and all remaining), q (quit).  Files
that received at least one replacement are saved automatically when
the session ends.

DIRECTORY defaults to the current directory."
  (interactive)
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (let* ((search-dir (or directory default-directory))
         (pattern (read-string "ast-grep pattern: " nil 'ast-grep-history))
         (rewrite (read-string (format "Rewrite `%s' with: " pattern)
                               nil 'ast-grep-rewrite-history))
         (matches (ast-grep--collect-rewrites pattern rewrite search-dir)))
    (if (null matches)
        (message "No matches for pattern: %s" pattern)
      (ast-grep--apply-rewrites matches))))

;;;###autoload
(defun ast-grep-rewrite-project ()
  "Rewrite ast-grep patterns across the current project.

Requires being in a project (detected via `project.el').
Equivalent to `ast-grep-rewrite' with the project root as directory."
  (interactive)
  (if-let ((project-root (ast-grep--project-root)))
      (ast-grep-rewrite project-root)
    (error "Not in a project")))

;;; Helper functions

(defun ast-grep--goto-match (match)
  "Go to the location specified by MATCH string.
MATCH should be in format \\='file:line:column:text\\='.
LINE is 1-indexed, COLUMN is 0-indexed (matching ast-grep's JSON output)."
  (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" match)
    (let ((file (match-string 1 match))
          (line (string-to-number (match-string 2 match)))
          (column (string-to-number (match-string 3 match))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))))

;;; Mode definition

;;;###autoload
(define-minor-mode ast-grep-mode
  "Minor mode for ast-grep integration."
  :lighter " ast-grep")

(provide 'ast-grep)

;;; ast-grep.el ends here
