;;; ast-grep.el --- Search code using ast-grep with completing-read interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Version: 0.1.0
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
;; - TODO: Asynchronous search for large codebases

;;; Code:

;; Declare external functions to avoid byte-compiler warnings
(declare-function project-current "project")
(declare-function project-root "project")

(defgroup ast-grep nil
  "Search code using ast-grep."
  :group 'tools
  :prefix "ast-grep-")

(defcustom ast-grep-executable "ast-grep"
  "Path to the ast-grep executable."
  :type 'string
  :group 'ast-grep)

(defcustom ast-grep-arguments '("--json")
  "Default arguments passed to ast-grep."
  :type '(repeat string)
  :group 'ast-grep)

(defcustom ast-grep-debug nil
  "Enable debug output for ast-grep commands.
When non-nil, ast-grep will output debug information including
the command being executed, working directory, and raw output."
  :type 'boolean
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
    (when (member "--json" ast-grep-arguments)
      (push "--json" args))
    (when directory
      ;; Expand ~ and environment variables in directory path
      (push (expand-file-name directory) args))
    (append cmd (reverse args))))

;;; Core functions

(defun ast-grep--run-command (pattern &optional directory)
  "Run ast-grep command with PATTERN in DIRECTORY."
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  
  (let ((default-directory (or directory default-directory))
        (command (ast-grep--build-command pattern directory)))
    ;; Debug output
    (when ast-grep-debug
      (message "Debug: Running command: %s" (mapconcat 'shell-quote-argument command " "))
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

;;; Interactive commands

;;;###autoload
(defun ast-grep-search (pattern &optional directory)
  "Search for ast-grep PATTERN in DIRECTORY.

PATTERN is an ast-grep pattern string (e.g., \\='$A && $A()\\=').
DIRECTORY defaults to current directory if not specified.

Interactively, prompts for pattern and uses current directory.
Results are displayed in a `completing-read' interface.
Selecting a result jumps to the match location.

Example patterns:
  \\='console.log($A)\\='     - Find console.log calls
  \\='$A && $A()\\='          - Find conditional function calls
  \\='function $NAME() {}\\=' - Find function declarations"
  (interactive (list (read-string "ast-grep pattern: " nil 'ast-grep-history)))
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  
  (let* ((search-dir (or directory default-directory))
         (candidates (ast-grep--candidates pattern search-dir)))
    (if candidates
        (let ((selection (completing-read
                          (format "ast-grep [%s]: " pattern)
                          candidates nil t nil 'ast-grep-history)))
          (when selection
            (ast-grep--goto-match selection)))
      (message "No matches found for pattern: %s" pattern))))

;;;###autoload
(defun ast-grep-project (pattern)
  "Search for ast-grep PATTERN in current project.

PATTERN is an ast-grep pattern string (e.g., \\='$A && $A()\\=').

Searches recursively from the project root directory.
Requires being in a project (detected via project.el).
Results are displayed in a `completing-read' interface.

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
Results are displayed in a `completing-read' interface."
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
