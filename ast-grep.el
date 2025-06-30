;;; ast-grep.el --- Search code using ast-grep with completing-read interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Version: 0.1.2
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

;; Declare external functions to avoid byte-compiler warnings
(declare-function project-current "project")
(declare-function project-root "project")
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

(defvar ast-grep--current-process nil
  "Current ast-grep process.")

;;; Utility functions

(defun ast-grep--executable-available-p ()
  "Check if ast-grep executable is available."
  (executable-find ast-grep-executable))

(defun ast-grep--project-root ()
  "Get the current project root directory."
  (when (require 'project nil t)
    (when-let ((project (project-current)))
      (project-root project))))

(defun ast-grep--build-command (pattern &optional directory)
  "Build ast-grep command with PATTERN and DIRECTORY."
  (let ((cmd (list ast-grep-executable "run"))
        (args '()))
    (push (format "--pattern=%s" pattern) args)
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

(defun ast-grep--async-source (directory)
  "Create async source for DIRECTORY using consult."
  (consult--async-pipeline
   (consult--async-throttle)
   (consult--async-process
    (lambda (input)
      (when (and input (>= (length input) ast-grep-async-min-input))
        (ast-grep--build-command input directory))))
   (consult--async-transform
    (lambda (items)
      (delq nil (mapcar #'ast-grep--parse-stream-line items))))))

(defun ast-grep--search-async (pattern directory)
  "Search asynchronously using consult for PATTERN in DIRECTORY."
  (let ((source (ast-grep--async-source directory)))
    (consult--read
     source
     :prompt (format "ast-grep [%s]: " pattern)
     :lookup #'consult--lookup-member
     :state (ast-grep--state)
     :category 'ast-grep
     :history 'ast-grep-history
     :require-match t
     :initial (format "#%s#" pattern))))

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
(defun ast-grep-search (pattern &optional directory)
  "Search for ast-grep PATTERN in DIRECTORY.

PATTERN is an ast-grep pattern string (e.g., \\='$A && $A()\\=').
DIRECTORY defaults to current directory if not specified.

Interactively, prompts for pattern and uses current directory.
Uses async search with `consult' if available.
Otherwise sync with `completing-read'.
Selecting a result jumps to the match location.

Example patterns:
  \\='console.log($A)\\='     - Find console.log calls
  \\='$A && $A()\\='          - Find conditional function calls
  \\='function $NAME() {}\\=' - Find function declarations"
  (interactive (list (read-string "ast-grep pattern: " nil 'ast-grep-history)))
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  
  (let ((search-dir (or directory default-directory)))
    (when-let ((selection
                (if (require 'consult nil t)
                    (ast-grep--search-async pattern search-dir)
                  (ast-grep--search-sync pattern search-dir))))
      (ast-grep--goto-match selection))))

;;;###autoload
(defun ast-grep-project (pattern)
  "Search for ast-grep PATTERN in current project.

PATTERN is an ast-grep pattern string (e.g., \\='$A && $A()\\=').

Searches recursively from the project root directory.
Requires being in a project (detected via project.el).
Uses async search with `consult' if available.
Otherwise sync with `completing-read'.

This is equivalent to `ast-grep-search' with project root as directory."
  (interactive (list (read-string "ast-grep pattern (project): " nil 'ast-grep-history)))
  (if-let ((project-root (ast-grep--project-root)))
      (ast-grep-search pattern project-root)
    (error "Not in a project")))

;;;###autoload
(defun ast-grep-directory (pattern directory)
  "Search for ast-grep PATTERN in specified DIRECTORY.

PATTERN is an ast-grep pattern string (e.g., \\='$A && $A()\\=').
DIRECTORY is the target directory path (supports ~ expansion).

Interactively, prompts for both pattern and directory.
Searches recursively from the specified directory.
Uses async search with `consult' if available.
Otherwise sync with `completing-read'."
  (interactive (list (read-string "ast-grep pattern: " nil 'ast-grep-history)
                     (read-directory-name "Directory: ")))
  (ast-grep-search pattern directory))

;;; Helper functions

(defun ast-grep--goto-match (match)
  "Go to the location specified by MATCH string.
MATCH should be in format \\='file:line:column:text\\='."
  (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" match)
    (let ((file (match-string 1 match))
          (line (string-to-number (match-string 2 match)))
          (column (string-to-number (match-string 3 match))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column (1- column)))))

;;; Mode definition

;;;###autoload
(define-minor-mode ast-grep-mode
  "Minor mode for ast-grep integration."
  :lighter " ast-grep")

(provide 'ast-grep)

;;; ast-grep.el ends here
