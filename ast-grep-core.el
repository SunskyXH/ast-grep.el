;;; ast-grep-core.el --- Shared internals for ast-grep.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 SunskyXH

;; Author: SunskyxXH <sunskyxh@gmail.com>
;; Keywords: tools, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core ast-grep command construction, process execution, JSON parsing,
;; candidate data, navigation, and rewrite support.  UI backend adapters
;; live in separate files.

;;; Code:

(require 'subr-x)

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

(defcustom ast-grep-use-nerd-icons t
  "When non-nil, prefix search candidates with nerd-icons file icons.
Requires the optional nerd-icons package."
  :type 'boolean
  :group 'ast-grep)

(defvar ast-grep-history nil
  "History list for ast-grep searches.")

(defvar ast-grep-rewrite-history nil
  "History list for ast-grep rewrite templates.")

(defvar ast-grep--candidate-table (make-hash-table :test 'equal)
  "Map display candidates to structured ast-grep match plists.")

(defconst ast-grep--match-property 'ast-grep-match
  "Text property that stores the structured match for a candidate.")

(defconst ast-grep--legacy-candidate-regexp
  "\\`\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"
  "Fallback regexp for legacy candidates without text properties.")

(defun ast-grep--executable-available-p ()
  "Check if ast-grep executable is available."
  (executable-find ast-grep-executable))

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
      (push (expand-file-name directory) args))
    (append cmd (reverse args))))

(defun ast-grep--command-string (command)
  "Return shell-quoted display text for COMMAND."
  (mapconcat #'shell-quote-argument command " "))

(defun ast-grep--read-file (file)
  "Return the contents of FILE, or an empty string if FILE is missing."
  (if (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    ""))

(defun ast-grep--call (command &optional directory label)
  "Run ast-grep COMMAND in DIRECTORY and return stdout.
LABEL is used only in debug output.  Non-zero exits raise an error
that includes both stdout and stderr when available."
  (unless (ast-grep--executable-available-p)
    (error "The ast-grep executable not found. Please install ast-grep"))
  (let ((default-directory (or directory default-directory))
        (stderr-file (make-temp-file "ast-grep-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (when ast-grep-debug
            (message "Debug: %s command: %s"
                     (or label "ast-grep")
                     (ast-grep--command-string command))
            (message "Debug: Working directory: %s" default-directory))
          (let* ((exit-code (apply #'call-process
                                   (car command) nil (list t stderr-file) nil
                                   (cdr command)))
                 (stdout (buffer-string))
                 (stderr (ast-grep--read-file stderr-file))
                 (details (string-trim
                           (string-join (delq nil
                                              (list (unless (string-empty-p stdout)
                                                      stdout)
                                                    (unless (string-empty-p stderr)
                                                      stderr)))
                                        "\n"))))
            (when ast-grep-debug
              (message "Debug: %s stdout: %s" (or label "ast-grep") stdout)
              (message "Debug: %s stderr: %s" (or label "ast-grep") stderr)
              (message "Debug: %s exit code: %d"
                       (or label "ast-grep") exit-code))
            (if (zerop exit-code)
                stdout
              (error "The ast-grep failed with exit code %d%s"
                     exit-code
                     (if (string-empty-p details)
                         ""
                       (concat ": " details))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun ast-grep--run-command (pattern &optional directory)
  "Run ast-grep command with PATTERN in DIRECTORY."
  (let ((command (ast-grep--build-command pattern directory)))
    (ast-grep--call command directory "search")))

(defun ast-grep--reset-candidate-table ()
  "Clear the candidate registry for a new search session."
  (clrhash ast-grep--candidate-table))

(defun ast-grep--match-from-json (result)
  "Return a normalized search match plist from parsed JSON RESULT."
  (let* ((file (plist-get result :file))
         (range (plist-get result :range))
         (start (plist-get range :start))
         (line (plist-get start :line))
         (column (plist-get start :column))
         (text (plist-get result :text)))
    (list :file file
          :start-line line
          :start-column column
          :text text)))

(defun ast-grep--candidate-display-text (text)
  "Return a single-line display form for match TEXT."
  (string-trim
   (replace-regexp-in-string "[\r\n]+" " " (or text ""))))

(defun ast-grep--nerd-icons-available-p ()
  "Return non-nil when nerd-icons file icons can be rendered."
  (and ast-grep-use-nerd-icons
       (require 'nerd-icons nil t)
       (fboundp 'nerd-icons-icon-for-file)))

(declare-function nerd-icons-icon-for-file "nerd-icons")

(defun ast-grep--candidate-icon-display (display file)
  "Return DISPLAY with an optional nerd-icons prefix for FILE.
The underlying candidate string stays DISPLAY so candidate lookup and
legacy parsing keep working; only the rendered form gains an icon."
  (if (ast-grep--nerd-icons-available-p)
      (propertize display
                  'display
                  (concat (nerd-icons-icon-for-file file) " " display))
    display))

(defun ast-grep--format-candidate (match)
  "Return a display candidate for MATCH and register its structured data."
  (let* ((file (plist-get match :file))
         (text (ast-grep--candidate-display-text (plist-get match :text)))
         (display (format "%s:%d:%d:%s"
                          file
                          (1+ (plist-get match :start-line))
                          (plist-get match :start-column)
                          text))
         (candidate (ast-grep--candidate-icon-display display file)))
    (puthash display match ast-grep--candidate-table)
    (add-text-properties 0 (length display)
                         (list ast-grep--match-property match)
                         candidate)
    candidate))

(defun ast-grep--legacy-candidate-match (candidate)
  "Parse legacy string CANDIDATE into a match plist."
  (when (string-match ast-grep--legacy-candidate-regexp candidate)
    (list :file (match-string-no-properties 1 candidate)
          :start-line (1- (string-to-number
                           (match-string-no-properties 2 candidate)))
          :start-column (string-to-number
                         (match-string-no-properties 3 candidate)))))

(defun ast-grep--candidate-match (candidate)
  "Return structured match data for CANDIDATE.
Candidates produced by this package carry the match as a text
property.  A hash-table lookup and legacy regexp fallback are kept so
that candidates survive backends that strip text properties."
  (cond
   ((and (consp candidate) (plist-get candidate :file))
    candidate)
   ((stringp candidate)
    (or (get-text-property 0 ast-grep--match-property candidate)
        (gethash (substring-no-properties candidate) ast-grep--candidate-table)
        (ast-grep--legacy-candidate-match candidate)))))

(defun ast-grep--goto-line-column (line column)
  "Move point to 0-based LINE and character-index COLUMN.
Both values match ast-grep's JSON output.  `forward-char' is
intentional: ast-grep reports character indices, while
`move-to-column' uses visual columns that change with tabs and
double-width characters."
  (goto-char (point-min))
  (forward-line line)
  (forward-char column))

(defun ast-grep--parse-stream-line (line)
  "Parse a single JSON LINE from streaming output."
  (when (and line (not (string-empty-p line)))
    (condition-case nil
        (ast-grep--format-candidate
         (ast-grep--match-from-json
          (json-parse-string line :object-type 'plist)))
      (error nil))))

(defun ast-grep--parse-stream-output (output)
  "Parse streaming JSON OUTPUT into a list of candidates."
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t)))
      (delq nil (mapcar #'ast-grep--parse-stream-line lines)))))

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
  (let* ((command (ast-grep--build-command pattern directory rewrite))
         (output (ast-grep--call command directory "rewrite")))
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
The prompt accepts y (yes), n (skip), ! (apply this and all
remaining), q (quit), following `query-replace' conventions.
Modified buffers are left for the user to save with
`save-some-buffers' (\\[save-some-buffers]), matching
`project-query-replace-regexp' behaviour."
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
                                  (format "Replace `%s' with `%s'? (y/n/!/q) "
                                          (plist-get m :text)
                                          (plist-get m :replacement))
                                  '(?y ?n ?! ?q))
                               (quit ?q)))))
                      (pcase choice
                        ((or ?y ?!)
                         (when (eq choice ?!) (setq all t))
                         (goto-char beg)
                         (delete-region beg end)
                         (insert (plist-get m :replacement))
                         (unless (memq buf modified-buffers)
                           (push buf modified-buffers))
                         (setq replaced (1+ replaced)))
                        (?n (setq skipped (1+ skipped)))
                        (?q (setq quit t)))))
                (delete-overlay overlay)))))))
    (message
     "%s"
     (substitute-command-keys
      (format "Replaced %d match(es) in %d file(s); skipped %d%s%s"
              replaced
              (length modified-buffers)
              skipped
              (if quit " (quit)" "")
              (if modified-buffers
                  "; use \\[save-some-buffers] to save"
                ""))))))

(defun ast-grep--goto-match (candidate)
  "Go to the location specified by CANDIDATE.
CANDIDATE may be a structured match plist, a propertized display
candidate, or a legacy `file:line:column:text' string."
  (when-let ((match (ast-grep--candidate-match candidate)))
    (find-file (plist-get match :file))
    (ast-grep--goto-line-column
     (plist-get match :start-line)
     (plist-get match :start-column))))

(provide 'ast-grep-core)

;;; ast-grep-core.el ends here
