;;; ast-grep-outline.el --- imenu index from ast-grep outline -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Build an imenu index for the current file from `ast-grep outline'.
;; Once installed as `imenu-create-index-function', every imenu consumer
;; (`imenu', `consult-imenu', `counsel-imenu', `helm-imenu', `imenu-list',
;; ...) can jump to the symbols ast-grep extracts.  The outline runs against
;; the file on disk, so positions are accurate for a saved buffer.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'ast-grep-core)

(declare-function consult-imenu "consult-imenu")
(declare-function counsel-imenu "counsel")
(declare-function helm-imenu "helm-imenu")
(defvar consult-imenu--cache)
(defvar helm-cached-imenu-alist)
(defvar helm-cached-imenu-candidates)
(defvar helm-cached-imenu-tick)
(defvar ivy-mode)
(defvar helm-mode)

(defconst ast-grep--outline-type-titles
  '(("class" . "Classes")
    ("interface" . "Interfaces")
    ("struct" . "Structs")
    ("enum" . "Enums")
    ("trait" . "Traits")
    ("object" . "Objects")
    ("module" . "Modules")
    ("namespace" . "Namespaces")
    ("function" . "Functions")
    ("method" . "Methods")
    ("constructor" . "Constructors")
    ("field" . "Fields")
    ("property" . "Properties")
    ("constant" . "Constants")
    ("variable" . "Variables")
    ("type" . "Types")
    ("macro" . "Macros"))
  "Map ast-grep outline symbol types to imenu group titles.
The order also fixes how groups are sorted in the index, so symbols of
the same kind always appear in the same place regardless of source order.")

(defun ast-grep--outline-group-title (type)
  "Return the imenu group title for outline symbol TYPE.
Unknown types fall back to a capitalized form of TYPE."
  (or (cdr (assoc type ast-grep--outline-type-titles))
      (capitalize (or type "Other"))))

(defun ast-grep--build-outline-command (file)
  "Build the `ast-grep outline' command for FILE."
  (list ast-grep-executable "outline" "--json=stream" (expand-file-name file)))

(defun ast-grep--run-outline (file)
  "Run `ast-grep outline' on FILE and return its raw stdout."
  (ast-grep--call (ast-grep--build-outline-command file) nil "outline"))

(defun ast-grep--outline-parse (output)
  "Parse outline stream OUTPUT into a flat list of top-level item plists.
OUTPUT holds one JSON object per file; items from every object are
collected so multi-file output degrades gracefully."
  (when (and output (not (string-empty-p output)))
    (apply #'append
           (delq nil
                 (mapcar
                  (lambda (line)
                    (condition-case nil
                        (plist-get (json-parse-string
                                    line :object-type 'plist :array-type 'list)
                                   :items)
                      (error nil)))
                  (split-string output "\n" t))))))

(defun ast-grep--outline-item-position (item)
  "Return the buffer position of outline ITEM's start, or nil.
The position is computed in the current buffer using the same
character-index coordinates ast-grep reports."
  (let* ((start (plist-get (plist-get item :range) :start))
         (line (plist-get start :line))
         (column (plist-get start :column)))
    (when (and line column)
      (save-excursion
        (ast-grep--goto-line-column line column)
        (point)))))

(defun ast-grep--outline-flatten (items prefix)
  "Return flat (TYPE NAME POSITION) entries for outline ITEMS.
Members are qualified with their enclosing item via PREFIX, e.g.
\"Widget.render\", and recursion preserves source order."
  (apply
   #'append
   (mapcar
    (lambda (item)
      (let* ((raw-name (plist-get item :name))
             (name (and (stringp raw-name) raw-name))
             (qualified (if (and prefix name) (concat prefix "." name) name))
             (pos (and name (ast-grep--outline-item-position item)))
             (self (when (and name pos)
                     (list (list (plist-get item :symbolType) qualified pos)))))
        (append self
                (ast-grep--outline-flatten (plist-get item :members) qualified))))
    items)))

(defun ast-grep--outline-dedupe-names (leaves)
  "Make duplicate display names in LEAVES unique so every entry is reachable.
imenu resolves a selection with `assoc', which only finds the first leaf
of a given name; later duplicates become \"NAME<2>\", \"NAME<3>\", ...
Positions are untouched."
  (let ((counts (make-hash-table :test 'equal)))
    (mapcar
     (lambda (leaf)
       (let ((seen (1+ (gethash (car leaf) counts 0))))
         (puthash (car leaf) seen counts)
         (if (= seen 1)
             leaf
           (cons (format "%s<%d>" (car leaf) seen) (cdr leaf)))))
     leaves)))

(defun ast-grep--outline-group (entries)
  "Group flat ENTRIES into an imenu index alist.
Each group is (TITLE (NAME . POS) ...); groups are ordered by
`ast-grep--outline-type-titles' and entries keep source order."
  (let ((groups nil)
        (order (mapcar #'cdr ast-grep--outline-type-titles)))
    (dolist (entry entries)
      (let* ((title (ast-grep--outline-group-title (nth 0 entry)))
             (cell (assoc title groups)))
        (if cell
            (setcdr cell (cons (cons (nth 1 entry) (nth 2 entry)) (cdr cell)))
          (push (cons title (list (cons (nth 1 entry) (nth 2 entry)))) groups))))
    (mapcar (lambda (group)
              (cons (car group)
                    (ast-grep--outline-dedupe-names (nreverse (cdr group)))))
            (sort (nreverse groups)
                  (lambda (a b)
                    (< (or (cl-position (car a) order :test #'equal)
                           most-positive-fixnum)
                       (or (cl-position (car b) order :test #'equal)
                           most-positive-fixnum)))))))

(defun ast-grep--outline-imenu-index ()
  "Build an imenu index for the current file using `ast-grep outline'.
Return nil when the buffer has no file or ast-grep finds no symbols.
A failing or too-old (<0.44.0) ast-grep degrades to an empty index
rather than signalling, since this runs from `imenu-create-index-function'."
  (when buffer-file-name
    (condition-case err
        (ast-grep--outline-group
         (ast-grep--outline-flatten
          (ast-grep--outline-parse (ast-grep--run-outline buffer-file-name))
          nil))
      (error
       (message "ast-grep outline failed: %s" (error-message-string err))
       nil))))

(defun ast-grep--outline-clear-helm-imenu-cache ()
  "Invalidate helm-imenu's buffer-local cache when helm-imenu is loaded."
  (dolist (cache '(helm-cached-imenu-alist
                   helm-cached-imenu-candidates
                   helm-cached-imenu-tick))
    (when (boundp cache)
      (kill-local-variable cache))))

(defun ast-grep--outline-invalidate-imenu-caches ()
  "Drop cached imenu indexes so a changed index source takes effect.
imenu caches in `imenu--index-alist'; consult-imenu and helm-imenu keep
their own buffer-local caches.  None of them notices a swapped
`imenu-create-index-function'."
  (when (local-variable-p 'imenu--index-alist)
    (setq-local imenu--index-alist nil))
  (when (boundp 'consult-imenu--cache)
    (kill-local-variable 'consult-imenu--cache))
  (ast-grep--outline-clear-helm-imenu-cache))

(defvar-local ast-grep--outline-saved-imenu-function 'unset
  "Saved `imenu-create-index-function' from before `ast-grep-outline-mode'.")

;;;###autoload
(define-minor-mode ast-grep-outline-mode
  "Use `ast-grep outline' as this buffer's imenu index source.
While enabled, `imenu', `consult-imenu', `counsel-imenu', `helm-imenu'
and other imenu consumers list the symbols ast-grep extracts from the
file on disk.  Save the buffer to keep positions accurate."
  :lighter " sg-outline"
  (if ast-grep-outline-mode
      (progn
        ;; Record the prior function only the first time we install ours, so a
        ;; re-entrant enable never captures `ast-grep--outline-imenu-index'
        ;; itself and disable can still restore the genuine prior value.
        (when (and (eq ast-grep--outline-saved-imenu-function 'unset)
                   (not (eq imenu-create-index-function
                            #'ast-grep--outline-imenu-index)))
          (setq ast-grep--outline-saved-imenu-function
                (if (local-variable-p 'imenu-create-index-function)
                    imenu-create-index-function
                  'unset)))
        (setq-local imenu-create-index-function
                    #'ast-grep--outline-imenu-index))
    (if (eq ast-grep--outline-saved-imenu-function 'unset)
        (kill-local-variable 'imenu-create-index-function)
      (setq-local imenu-create-index-function
                  ast-grep--outline-saved-imenu-function))
    (setq ast-grep--outline-saved-imenu-function 'unset))
  ;; Either toggle changes the index source; drop caches that still reflect
  ;; the previous one so imenu and consult-imenu rebuild against it.
  (ast-grep--outline-invalidate-imenu-caches))

;;;###autoload
(defun ast-grep-outline ()
  "Jump to a symbol in the current file via `ast-grep outline'.
Pick the picker the way `ast-grep-search' picks a backend: when `ivy-mode'
is active use `counsel-imenu' (or the built-in `imenu' if counsel is
unavailable, never `consult-imenu'); when `helm-mode' is active use
`helm-imenu' (or the built-in `imenu' if Helm is unavailable, never
`consult-imenu'); otherwise use `consult-imenu' when available, else
`imenu'.  This is a one-shot entry point: it does not require
`ast-grep-outline-mode' and leaves the buffer's own imenu configuration
untouched."
  (interactive)
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((origin-buffer (current-buffer))
        (saved-fn (if (local-variable-p 'imenu-create-index-function)
                      imenu-create-index-function
                    'unset))
        (saved-alist imenu--index-alist)
        (cache-local (and (boundp 'consult-imenu--cache)
                          (local-variable-p 'consult-imenu--cache)))
        (saved-cache (and (boundp 'consult-imenu--cache) consult-imenu--cache))
        helm-cache-touched
        (imenu-auto-rescan t))
    (unwind-protect
        (progn
          (setq-local imenu-create-index-function
                      #'ast-grep--outline-imenu-index)
          (setq imenu--index-alist nil)
          (cond
           ;; Mirror `ast-grep--select-backend': active UI modes own their
           ;; imenu picker and never fall through to consult.
           ((bound-and-true-p ivy-mode)
            (if (and (require 'counsel nil t) (fboundp 'counsel-imenu))
                (call-interactively #'counsel-imenu)
              (call-interactively #'imenu)))
           ;; Under helm-mode, use helm-imenu if available; otherwise fall
           ;; back to built-in imenu, never consult.
           ((bound-and-true-p helm-mode)
            (if (and (require 'helm-imenu nil t) (fboundp 'helm-imenu))
                (progn
                  (setq helm-cache-touched t)
                  (ast-grep--outline-clear-helm-imenu-cache)
                  (call-interactively #'helm-imenu))
              (call-interactively #'imenu)))
           ((and (require 'consult-imenu nil t) (fboundp 'consult-imenu))
            ;; consult-imenu caches per `buffer-modified-tick' and ignores
            ;; `imenu--index-alist', so clear its cache to force a recompute
            ;; against our index.
            (setq-local consult-imenu--cache nil)
            (call-interactively #'consult-imenu))
           (t (call-interactively #'imenu))))
      ;; A picker action may leave another buffer current when it returns
      ;; (e.g. helm-imenu's quit-and-find-file on C-x C-f), so pin the
      ;; cleanup to the buffer whose imenu state was swapped.
      (when (buffer-live-p origin-buffer)
        (with-current-buffer origin-buffer
          (if (eq saved-fn 'unset)
              (kill-local-variable 'imenu-create-index-function)
            (setq-local imenu-create-index-function saved-fn))
          (setq imenu--index-alist saved-alist)
          (when (boundp 'consult-imenu--cache)
            (if cache-local
                (setq-local consult-imenu--cache saved-cache)
              (kill-local-variable 'consult-imenu--cache)))
          (when helm-cache-touched
            (ast-grep--outline-clear-helm-imenu-cache)))))))

(provide 'ast-grep-outline)

;;; ast-grep-outline.el ends here
