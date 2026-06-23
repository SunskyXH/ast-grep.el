;;; ast-grep.el --- Search code using ast-grep with completing-read interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyXH <sunskyxh@gmail.com>
;; Version: 0.4.1
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
;; - Async search with live results (consult, Helm, or counsel/ivy when
;;   `ivy-mode' is active)

;;; Code:

(require 'project)
(require 'ast-grep-core)
(require 'ast-grep-sync)

(defvar ivy-mode nil)
(defvar helm-mode nil)

(declare-function ast-grep--consult-available-p "ast-grep-consult")
(declare-function ast-grep--search-consult "ast-grep-consult")
(declare-function ast-grep--ivy-available-p "ast-grep-ivy")
(declare-function ast-grep--search-ivy "ast-grep-ivy")
(declare-function ast-grep--helm-available-p "ast-grep-helm")
(declare-function ast-grep--search-helm "ast-grep-helm")

(defcustom ast-grep-search-backend 'auto
  "Backend selector for `ast-grep-search'.
`auto' (default) picks the best available backend:
 - counsel/ivy async when `ivy-mode' is active and counsel is
   loadable;
 - Helm async when `helm-mode' is active and Helm is loadable;
 - consult async when consult is loadable and neither `ivy-mode' nor
   `helm-mode' is active;
 - synchronous `completing-read' otherwise.
`consult' forces the consult async pipeline; falls back to sync if
consult is not loadable.
`ivy' forces the counsel/ivy async pipeline; falls back to sync if
counsel/ivy is not loadable.
`helm' forces the Helm async pipeline; falls back to sync if Helm is
not loadable.
`sync' forces the synchronous `completing-read' path."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Consult async" consult)
                 (const :tag "Counsel/ivy async" ivy)
                 (const :tag "Helm async" helm)
                 (const :tag "Sync completing-read" sync))
  :group 'ast-grep)

(defun ast-grep--consult-backend-available-p ()
  "Return non-nil when the consult backend and consult are loadable."
  (and (require 'ast-grep-consult nil t)
       (ast-grep--consult-available-p)))

(defun ast-grep--ivy-backend-available-p ()
  "Return non-nil when the ivy backend and ivy/counsel are loadable."
  (and (require 'ast-grep-ivy nil t)
       (ast-grep--ivy-available-p)))

(defun ast-grep--helm-backend-available-p ()
  "Return non-nil when the Helm backend and Helm are loadable."
  (and (require 'ast-grep-helm nil t)
       (ast-grep--helm-available-p)))

(defun ast-grep--project-root ()
  "Get the current project root directory."
  (when-let ((project (project-current)))
    (project-root project)))

(defun ast-grep--select-backend ()
  "Return the resolved backend symbol: `consult', `ivy', `helm', or `sync'.
Honours `ast-grep-search-backend'.  In `auto' mode, active
`ivy-mode' or `helm-mode' suppresses consult because consult async
minibuffer hooks and those UIs' minibuffer management are not
compatible."
  (pcase ast-grep-search-backend
    ('sync 'sync)
    ('consult (if (ast-grep--consult-backend-available-p) 'consult 'sync))
    ('ivy (if (ast-grep--ivy-backend-available-p) 'ivy 'sync))
    ('helm (if (ast-grep--helm-backend-available-p) 'helm 'sync))
    ('auto
     (if (bound-and-true-p ivy-mode)
         (if (ast-grep--ivy-backend-available-p) 'ivy 'sync)
       (if (bound-and-true-p helm-mode)
           (if (ast-grep--helm-backend-available-p) 'helm 'sync)
         (if (ast-grep--consult-backend-available-p) 'consult 'sync))))))

(defun ast-grep--run-search-backend (backend directory)
  "Run BACKEND for ast-grep search in DIRECTORY."
  (pcase backend
    ('consult
     (require 'ast-grep-consult)
     (ast-grep--search-consult directory))
    ('ivy
     (require 'ast-grep-ivy)
     (ast-grep--search-ivy directory))
    ('helm
     (require 'ast-grep-helm)
     (ast-grep--search-helm directory))
    ('sync (ast-grep--search-sync directory))))

(defun ast-grep--backend-description ()
  "Return a human-readable description of the active search backend."
  (let ((resolved (ast-grep--select-backend)))
    (if (eq ast-grep-search-backend resolved)
        (format "ast-grep backend: %s" resolved)
      (format "ast-grep backend: %s -> %s"
              ast-grep-search-backend
              resolved))))

;;; Interactive commands

;;;###autoload
(defun ast-grep-describe-backend ()
  "Display the configured and resolved backend for `ast-grep-search'."
  (interactive)
  (let ((description (ast-grep--backend-description)))
    (message "%s" description)
    description))

;;;###autoload
(defun ast-grep-search (&optional directory)
  "Search for ast-grep patterns in DIRECTORY.

DIRECTORY defaults to current directory.

The backend is chosen by `ast-grep-search-backend'.  In the default
`auto' mode you get the counsel/ivy async pipeline when `ivy-mode' is
active and counsel is available, the Helm async pipeline when
`helm-mode' is active and Helm is available, the consult async pipeline
when consult is available and neither UI mode is active, and a
synchronous `completing-read' fallback otherwise.

Example patterns:
  console.log($A)     - Find console.log calls
  $A && $A()          - Find conditional function calls
  function $NAME() {} - Find function declarations"
  (interactive)
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (let ((search-dir (or directory default-directory)))
    (ast-grep--run-search-backend
     (ast-grep--select-backend)
     search-dir)))

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
(skip), ! (apply this and all remaining), q (quit).  Modified
buffers are left for the user to save with `save-some-buffers'.

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

;;; Mode definition

;;;###autoload
(define-minor-mode ast-grep-mode
  "Minor mode for ast-grep integration."
  :lighter " ast-grep")

(provide 'ast-grep)

;;; ast-grep.el ends here
