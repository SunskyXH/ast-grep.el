;;; ast-grep-test.el --- Tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for ast-grep.el using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ast-grep)

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

;;; Utility function tests

(ert-deftest ast-grep-test-executable-available-p ()
  "Test executable availability check."
  (let ((ast-grep-executable "nonexistent-command"))
    (should-not (ast-grep--executable-available-p)))
  (let ((ast-grep-executable "echo"))
    (should (ast-grep--executable-available-p))))

(ert-deftest ast-grep-test-build-command ()
  "Test command building."
  (let ((ast-grep-executable "ast-grep"))
    (should (equal (ast-grep--build-command "pattern")
                   '("ast-grep" "run" "--pattern=pattern" "--json=stream")))
    (should (equal (ast-grep--build-command "pattern" "/path")
                   '("ast-grep" "run" "--pattern=pattern" "--json=stream" "/path")))))

(ert-deftest ast-grep-test-build-command-with-rewrite ()
  "Command should include --rewrite when REWRITE is provided."
  (let ((ast-grep-executable "ast-grep"))
    (should (equal (ast-grep--build-command "pat" nil "fix")
                   '("ast-grep" "run" "--pattern=pat" "--rewrite=fix" "--json=stream")))
    (should (equal (ast-grep--build-command "pat" "/path" "fix")
                   '("ast-grep" "run" "--pattern=pat" "--rewrite=fix"
                     "--json=stream" "/path")))))

(ert-deftest ast-grep-test-parse-json-output ()
  "Test JSON output parsing."
  (let ((json-output "[{\"file\":\"test.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"console.log()\"}]"))
    (should (equal (ast-grep--parse-json-output json-output)
                   '("test.js:1:0:console.log()"))))
  (should-not (ast-grep--parse-json-output ""))
  (should-not (ast-grep--parse-json-output nil)))

(ert-deftest ast-grep-test-parse-stream-line ()
  "Test parsing of a single JSON line from streaming output."
  (let ((line "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":3,\"column\":7}},\"text\":\"  hit \"}"))
    (should (equal (ast-grep--parse-stream-line line)
                   "a.js:4:7:hit")))
  (should-not (ast-grep--parse-stream-line nil))
  (should-not (ast-grep--parse-stream-line ""))
  (should-not (ast-grep--parse-stream-line "not-json")))

(ert-deftest ast-grep-test-parse-stream-output ()
  "Test parsing of multi-line streaming output, skipping malformed lines."
  (let ((output (concat "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x\"}\n"
                        "garbage\n"
                        "{\"file\":\"b.js\",\"range\":{\"start\":{\"line\":2,\"column\":4}},\"text\":\"y\"}\n")))
    (should (equal (ast-grep--parse-stream-output output)
                   '("a.js:1:0:x" "b.js:3:4:y"))))
  (should-not (ast-grep--parse-stream-output nil))
  (should-not (ast-grep--parse-stream-output "")))

;;; Rewrite parser tests

(ert-deftest ast-grep-test-parse-rewrite-line ()
  "Parse a JSON line that includes end position and replacement."
  (let* ((line (concat "{\"file\":\"a.js\","
                       "\"range\":{\"start\":{\"line\":0,\"column\":0},"
                       "\"end\":{\"line\":0,\"column\":13}},"
                       "\"text\":\"console.log()\","
                       "\"replacement\":\"logger.info()\"}"))
         (m (ast-grep--parse-rewrite-line line)))
    (should (equal (plist-get m :file) "a.js"))
    (should (= (plist-get m :start-line) 0))
    (should (= (plist-get m :start-column) 0))
    (should (= (plist-get m :end-line) 0))
    (should (= (plist-get m :end-column) 13))
    (should (equal (plist-get m :text) "console.log()"))
    (should (equal (plist-get m :replacement) "logger.info()")))
  (should-not (ast-grep--parse-rewrite-line nil))
  (should-not (ast-grep--parse-rewrite-line ""))
  (should-not (ast-grep--parse-rewrite-line "not-json")))

(ert-deftest ast-grep-test-rewrite-sort-reverses-within-file ()
  "Matches in the same file are sorted in reverse position order."
  (let* ((a (list :file "a.js" :start-line 0 :start-column 0))
         (b (list :file "a.js" :start-line 2 :start-column 4))
         (c (list :file "a.js" :start-line 2 :start-column 8))
         (d (list :file "b.js" :start-line 0 :start-column 0))
         (sorted (ast-grep--rewrite-sort (list a b c d))))
    (should (equal sorted (list c b a d)))))

(ert-deftest ast-grep-test-match-region-uses-character-columns ()
  "Region must be computed by character index, not visual column.
A tab at the start of the line would offset `move-to-column' but
`forward-char' lands on the correct character."
  (with-temp-buffer
    (insert "\tconsole.log(x);\n")
    (let* ((match (list :start-line 0 :start-column 1
                        :end-line 0 :end-column 15))
           (region (ast-grep--match-region match)))
      ;; start-column 1 must land just after the tab (position 2 in
      ;; 1-indexed buffer coords), not in the middle of the tab's
      ;; visual span.
      (should (= (car region) 2))
      (should (equal (buffer-substring (car region) (cdr region))
                     "console.log(x)")))))

;;; Async builder tests

(ert-deftest ast-grep-test-async-builder-respects-min-input ()
  "Builder returns nil when input is shorter than `ast-grep-async-min-input'."
  (let ((ast-grep-async-min-input 3)
        (ast-grep-executable "ast-grep"))
    (should-not (ast-grep--async-builder "" "/dir"))
    (should-not (ast-grep--async-builder "ab" "/dir"))
    (should (equal (ast-grep--async-builder "abc" "/dir")
                   '("ast-grep" "run" "--pattern=abc" "--json=stream" "/dir")))))

;;; goto-match tests

(ert-deftest ast-grep-test-goto-match-regex ()
  "Goto-match should extract line and column fields from MATCH string."
  (let ((match "buffer:2:2:line 2"))
    (should (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" match))
    (should (equal (match-string 2 match) "2"))
    (should (equal (match-string 3 match) "2"))))

(ert-deftest ast-grep-test-goto-match-real ()
  "Verify goto-match opens the file and moves point to the correct position."
  (skip-unless (file-exists-p ast-grep-test--sample-file))
  (let ((buffer-before (find-buffer-visiting ast-grep-test--sample-file)))
    (unwind-protect
        (progn
          (ast-grep--goto-match
           (format "%s:5:2:console.log(name)" ast-grep-test--sample-file))
          (should (equal (buffer-file-name) ast-grep-test--sample-file))
          (should (= (line-number-at-pos) 5))
          (should (= (current-column) 2))
          (should (looking-at-p "console\\.log(name)")))
      (unless buffer-before
        (when-let ((buf (find-buffer-visiting ast-grep-test--sample-file)))
          (kill-buffer buf))))))

;;; Candidates (mocked) tests

(ert-deftest ast-grep-test-candidates-empty-pattern ()
  "Test candidates function with empty pattern."
  (should-not (ast-grep--candidates ""))
  (should-not (ast-grep--candidates nil)))

(ert-deftest ast-grep-test-candidates-with-mock ()
  "Test candidates function with mocked ast-grep command."
  (cl-letf (((symbol-function 'ast-grep--run-command)
             (lambda (&rest _args)
               "[{\"file\":\"test.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"console.log()\"}]")))
    (let ((candidates (ast-grep--candidates "console.log")))
      (should (equal candidates '("test.js:1:0:console.log()"))))))

(ert-deftest ast-grep-test-candidates-with-error ()
  "Test candidates function with command error."
  (cl-letf (((symbol-function 'ast-grep--run-command)
             (lambda (&rest _args)
               (error "Command failed"))))
    (let ((candidates (ast-grep--candidates "pattern")))
      (should-not candidates))))

;;; End-to-end (require ast-grep binary)

(ert-deftest ast-grep-test-end-to-end-sync ()
  "Run ast-grep against the fixture and verify sync parsed candidates."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((output (ast-grep--run-command "console.log($A)"
                                        ast-grep-test--fixtures-dir))
         (candidates (ast-grep--parse-stream-output output)))
    (should (= 3 (length candidates)))
    (cl-loop for suffix in '(":1:0:console.log(\"hello\")"
                             ":3:0:console.log(x)"
                             ":5:2:console.log(name)")
             for cand in candidates
             do (should (string-suffix-p suffix cand)))))

(ert-deftest ast-grep-test-end-to-end-async-parser-matches-sync ()
  "Async per-line parser must produce the same candidates as the sync parser."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((output (ast-grep--run-command "console.log($A)"
                                        ast-grep-test--fixtures-dir))
         (sync (ast-grep--parse-stream-output output))
         (async (delq nil
                      (mapcar #'ast-grep--parse-stream-line
                              (split-string output "\n" t)))))
    (should (equal sync async))))

(ert-deftest ast-grep-test-end-to-end-goto-from-candidate ()
  "Pipe a real ast-grep candidate through goto-match and land at the match."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((output (ast-grep--run-command "console.log($A)"
                                        ast-grep-test--fixtures-dir))
         (candidates (ast-grep--parse-stream-output output))
         (third (nth 2 candidates))
         (buffer-before (find-buffer-visiting ast-grep-test--sample-file)))
    (should (stringp third))
    (unwind-protect
        (progn
          (ast-grep--goto-match third)
          (should (equal (buffer-file-name) ast-grep-test--sample-file))
          (should (= (line-number-at-pos) 5))
          (should (= (current-column) 2))
          (should (looking-at-p "console\\.log(name)")))
      (unless buffer-before
        (when-let ((buf (find-buffer-visiting ast-grep-test--sample-file)))
          (kill-buffer buf))))))

(ert-deftest ast-grep-test-end-to-end-async-builder-produces-runnable-command ()
  "The command built by the async pipeline matches the sync command."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((ast-grep-async-min-input 3)
         (input "console.log($A)")
         (built (ast-grep--async-builder input ast-grep-test--fixtures-dir))
         (output (with-temp-buffer
                   (apply #'call-process (car built) nil t nil (cdr built))
                   (buffer-string)))
         (candidates (ast-grep--parse-stream-output output)))
    (should (= 3 (length candidates)))))

;;; End-to-end async pipeline (require consult + ast-grep)

(ert-deftest ast-grep-test-end-to-end-async-pipeline ()
  "Drive the real consult pipeline: throttle -> process -> transform.
Spawns a real ast-grep subprocess and verifies that candidates flow
through every stage to the sink."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (skip-unless ast-grep-test--consult-available)
  (let* ((ast-grep-async-min-input 3)
         (source (ast-grep--async-source ast-grep-test--fixtures-dir))
         (candidates (ast-grep-test--drive-async source "console.log($A)")))
    (should (= 3 (length candidates)))
    (cl-loop for suffix in '(":1:0:console.log(\"hello\")"
                             ":3:0:console.log(x)"
                             ":5:2:console.log(name)")
             do (should (cl-some (lambda (c) (string-suffix-p suffix c))
                                 candidates)))))

(ert-deftest ast-grep-test-end-to-end-async-pipeline-min-input ()
  "Pipeline stays quiet when input is below the min-input threshold."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (skip-unless ast-grep-test--consult-available)
  (let* ((ast-grep-async-min-input 3)
         (source (ast-grep--async-source ast-grep-test--fixtures-dir))
         ;; Drive with a short pattern; the builder gates spawning,
         ;; so no subprocess starts and no candidates arrive.
         (candidates (ast-grep-test--drive-async source "ab" 1.0)))
    (should (null candidates))))

;;; ast-grep-search integration with consult

(ert-deftest ast-grep-test-search-with-consult-wires-goto-match ()
  "With consult available `ast-grep-search' should hand the selected
candidate to `ast-grep--goto-match'.  `consult--read' is mocked so the
test does not depend on an interactive minibuffer."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (skip-unless ast-grep-test--consult-available)
  (let* ((target ast-grep-test--sample-file)
         (selection (format "%s:5:2:console.log(name)" target))
         (calls 0)
         (buffer-before (find-buffer-visiting target)))
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _args)
                 (cl-incf calls)
                 selection)))
      (unwind-protect
          (progn
            (ast-grep-search ast-grep-test--fixtures-dir)
            (should (= 1 calls))
            (should (equal (buffer-file-name) target))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2))
            (should (looking-at-p "console\\.log(name)")))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting target)))
            (kill-buffer buf)))))))

(ert-deftest ast-grep-test-search-without-consult-uses-sync-path ()
  "Without consult `ast-grep-search' prompts for the pattern up front
and routes the chosen candidate through `ast-grep--goto-match'."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((target ast-grep-test--sample-file)
         (selection (format "%s:5:2:console.log(name)" target))
         (read-string-calls 0)
         (buffer-before (find-buffer-visiting target)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (if (eq feature 'consult) nil t)))
              ((symbol-function 'read-string)
               (lambda (&rest _args)
                 (cl-incf read-string-calls)
                 "console.log($A)"))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) selection)))
      (unwind-protect
          (progn
            (ast-grep-search ast-grep-test--fixtures-dir)
            (should (= 1 read-string-calls))
            (should (equal (buffer-file-name) target))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2)))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting target)))
            (kill-buffer buf)))))))

;;; Rewrite end-to-end

(ert-deftest ast-grep-test-end-to-end-collect-rewrites ()
  "Collect rewrites against the JS fixture and verify shape."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let ((matches (ast-grep--collect-rewrites
                  "console.log($A)"
                  "logger.info($A)"
                  ast-grep-test--fixtures-dir)))
    (should (= 3 (length matches)))
    (dolist (m matches)
      (should (stringp (plist-get m :file)))
      (should (integerp (plist-get m :start-line)))
      (should (integerp (plist-get m :end-column)))
      (should (string-match-p "\\`logger\\.info"
                              (plist-get m :replacement))))))

(ert-deftest ast-grep-test-end-to-end-rewrite-applies-yes-to-all ()
  "Driving the prompt with `!' rewrites every match and saves the file."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((tmp-dir (make-temp-file "ast-grep-rewrite-" t))
         (tmp-file (expand-file-name "sample.js" tmp-dir))
         buf)
    (unwind-protect
        (progn
          (copy-file ast-grep-test--sample-file tmp-file)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (cond
                        ((string-prefix-p "ast-grep pattern" prompt)
                         "console.log($A)")
                        ((string-prefix-p "Rewrite" prompt)
                         "logger.info($A)"))))
                    ((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?!))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) b)))
            (ast-grep-rewrite tmp-dir))
          (setq buf (find-buffer-visiting tmp-file))
          (should buf)
          ;; The buffer should be saved (not modified).
          (with-current-buffer buf
            (should-not (buffer-modified-p)))
          ;; And the file on disk should reflect the rewrite.
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "logger\\.info(\"hello\")" content))
              (should (string-match-p "logger\\.info(x)" content))
              (should (string-match-p "logger\\.info(name)" content))
              (should-not (string-match-p "console\\.log" content)))))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (delete-directory tmp-dir t))))

(ert-deftest ast-grep-test-end-to-end-rewrite-quit-skips-remaining ()
  "Pressing `q' at the first match leaves the file untouched."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((tmp-dir (make-temp-file "ast-grep-rewrite-" t))
         (tmp-file (expand-file-name "sample.js" tmp-dir))
         buf)
    (unwind-protect
        (progn
          (copy-file ast-grep-test--sample-file tmp-file)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (cond
                        ((string-prefix-p "ast-grep pattern" prompt)
                         "console.log($A)")
                        ((string-prefix-p "Rewrite" prompt)
                         "logger.info($A)"))))
                    ((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?q))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) b)))
            (ast-grep-rewrite tmp-dir))
          (setq buf (find-buffer-visiting tmp-file))
          (when buf
            (with-current-buffer buf
              (should-not (buffer-modified-p))
              (should (string-match-p "console\\.log(\"hello\")"
                                      (buffer-string))))))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (delete-directory tmp-dir t))))

(provide 'test-ast-grep)

;;; ast-grep-test.el ends here
