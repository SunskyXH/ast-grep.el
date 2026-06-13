;;; ast-grep-test-helper.el --- Shared test helpers for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared fixtures and helper functions for ast-grep.el tests.

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Declare ivy-mode as a special variable so `let' bindings in tests are
;; dynamic (visible to `bound-and-true-p').  Without this, lexical-binding
;; makes the binding invisible to the backend selector.
(defvar ivy-mode nil)
(defvar consult-async-input-throttle)
(defvar consult-async-input-debounce)
(defvar counsel-async-command-delay)
(defvar vc-handled-backends)

;; Tests open fixture and temporary files in batch mode; VC refresh hooks add
;; unrelated backend autoload state and can fail before assertions run.
(setq vc-handled-backends nil)

(defconst ast-grep-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing test fixture source files.")

(defconst ast-grep-test--sample-file
  (expand-file-name "sample.js" ast-grep-test--fixtures-dir))

(defun ast-grep-test--ast-grep-available-p ()
  "Return non-nil when the ast-grep binary is on PATH."
  (executable-find "ast-grep"))

(defvar ast-grep-test--consult-available
  (require 'consult nil t)
  "Non-nil if consult is loadable during this test run.")

(defvar ast-grep-test--ivy-available
  (and (require 'ivy nil t)
       (require 'counsel nil t))
  "Non-nil if ivy and counsel are loadable during this test run.")

(defun ast-grep-test--drive-async (source pattern &optional timeout)
  "Drive consult async SOURCE with PATTERN, return collected candidates.
Uses a permissive sink that ignores actions other than candidate lists
\(indicator vectors and lifecycle actions are skipped, mirroring what
the indicator/refresh stages of `consult--read' would absorb\).
TIMEOUT defaults to 5 seconds."
  (let* ((consult-async-input-throttle 0)
         (consult-async-input-debounce 0)
         (collected nil)
         (sink (lambda (action)
                 (when (consp action)
                   (setq collected (append collected action)))))
         (entry (funcall source sink))
         (deadline (+ (float-time) (or timeout 5.0))))
    (unwind-protect
        (progn
          (funcall entry 'setup)
          (funcall entry pattern)
          (while (and (< (float-time) deadline)
                      (< (length collected) 3))
            (accept-process-output nil 0.05)
            (sit-for 0.05))
          collected)
      (funcall entry 'destroy))))

(provide 'ast-grep-test-helper)

;;; ast-grep-test-helper.el ends here
