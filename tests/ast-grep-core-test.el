;;; ast-grep-core-test.el --- Core tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-core)

(ert-deftest ast-grep-core-test-executable-available-p ()
  "Test executable availability check."
  (let ((ast-grep-executable "nonexistent-command"))
    (should-not (ast-grep--executable-available-p)))
  (let ((ast-grep-executable "echo"))
    (should (ast-grep--executable-available-p))))

(ert-deftest ast-grep-core-test-build-command ()
  "Test command building."
  (let ((ast-grep-executable "ast-grep"))
    (should (equal (ast-grep--build-command "pattern")
                   '("ast-grep" "run" "--pattern=pattern" "--json=stream")))
    (should (equal (ast-grep--build-command "pattern" "/path")
                   '("ast-grep" "run" "--pattern=pattern" "--json=stream" "/path")))))

(ert-deftest ast-grep-core-test-build-command-with-rewrite ()
  "Command should include --rewrite when REWRITE is provided."
  (let ((ast-grep-executable "ast-grep"))
    (should (equal (ast-grep--build-command "pat" nil "fix")
                   '("ast-grep" "run" "--pattern=pat" "--rewrite=fix" "--json=stream")))
    (should (equal (ast-grep--build-command "pat" "/path" "fix")
                   '("ast-grep" "run" "--pattern=pat" "--rewrite=fix"
                     "--json=stream" "/path")))))

(ert-deftest ast-grep-core-test-parse-stream-line ()
  "Test parsing of a single JSON line from streaming output."
  (let* ((line "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":3,\"column\":7}},\"text\":\"  hit \"}")
         (candidate (ast-grep--parse-stream-line line))
         (match (ast-grep--candidate-match candidate)))
    (should (equal candidate "a.js:4:7:hit"))
    (should (equal (plist-get match :file) "a.js"))
    (should (= (plist-get match :start-line) 3))
    (should (= (plist-get match :start-column) 7))
    (should (equal (plist-get match :text) "  hit ")))
  (should-not (ast-grep--parse-stream-line nil))
  (should-not (ast-grep--parse-stream-line ""))
  (should-not (ast-grep--parse-stream-line "not-json")))

(ert-deftest ast-grep-core-test-format-candidate-keeps-display-single-line ()
  "Multiline match text must not break backend newline protocols."
  (let* ((line "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"foo\\nbar\"}")
         (candidate (ast-grep--parse-stream-line line))
         (match (ast-grep--candidate-match candidate)))
    (should (equal candidate "a.js:1:0:foo bar"))
    (should-not (string-match-p "\n" candidate))
    (should (equal (plist-get match :text) "foo\nbar"))
    (should (eq (ast-grep--candidate-match
                 (substring-no-properties candidate))
                match))))

(ert-deftest ast-grep-core-test-format-candidate-keeps-structured-match ()
  "Display text is not the data contract; the match plist is."
  (let* ((match (list :file "C:/tmp/a:b.js"
                      :start-line 4
                      :start-column 2
                      :text "x:y"))
         (candidate (ast-grep--format-candidate match)))
    (should (equal candidate "C:/tmp/a:b.js:5:2:x:y"))
    (should (eq (ast-grep--candidate-match candidate) match))
    ;; Simulate backends that strip text properties.
    (should (eq (ast-grep--candidate-match
                 (substring-no-properties candidate))
                match))))

(ert-deftest ast-grep-core-test-parse-stream-output ()
  "Test parsing of multi-line streaming output, skipping malformed lines."
  (let ((output (concat "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x\"}\n"
                        "garbage\n"
                        "{\"file\":\"b.js\",\"range\":{\"start\":{\"line\":2,\"column\":4}},\"text\":\"y\"}\n")))
    (should (equal (ast-grep--parse-stream-output output)
                   '("a.js:1:0:x" "b.js:3:4:y"))))
  (should-not (ast-grep--parse-stream-output nil))
  (should-not (ast-grep--parse-stream-output "")))

(ert-deftest ast-grep-core-test-parse-rewrite-line ()
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

(ert-deftest ast-grep-core-test-rewrite-sort-reverses-within-file ()
  "Matches in the same file are sorted in reverse position order."
  (let* ((a (list :file "a.js" :start-line 0 :start-column 0))
         (b (list :file "a.js" :start-line 2 :start-column 4))
         (c (list :file "a.js" :start-line 2 :start-column 8))
         (d (list :file "b.js" :start-line 0 :start-column 0))
         (sorted (ast-grep--rewrite-sort (list a b c d))))
    (should (equal sorted (list c b a d)))))

(ert-deftest ast-grep-core-test-match-region-uses-character-columns ()
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

(ert-deftest ast-grep-core-test-legacy-candidate-match ()
  "Legacy plain strings remain supported as a fallback."
  (let ((match (ast-grep--candidate-match "buffer:2:2:line 2")))
    (should (equal (plist-get match :file) "buffer"))
    (should (= (plist-get match :start-line) 1))
    (should (= (plist-get match :start-column) 2))))

(ert-deftest ast-grep-core-test-goto-match-real ()
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

(ert-deftest ast-grep-core-test-goto-match-uses-character-columns ()
  "Search navigation must use ast-grep character columns, not visual columns."
  (let* ((tmp-file (make-temp-file "ast-grep-tab-" nil ".js"))
         (buffer-before nil))
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert "\tconsole.log(x);\n"))
          (setq buffer-before (find-buffer-visiting tmp-file))
          (let ((candidate
                 (ast-grep--format-candidate
                  (list :file tmp-file
                        :start-line 0
                        :start-column 1
                        :text "console.log(x)"))))
            (ast-grep--goto-match candidate)
            ;; Buffer position 2 is immediately after the tab.  A visual-column
            ;; implementation would land before or inside the tab's display span.
            (should (= (point) 2))
            (should (looking-at-p "console\\.log(x)"))))
      (unless buffer-before
        (when-let ((buf (find-buffer-visiting tmp-file)))
          (kill-buffer buf)))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(ert-deftest ast-grep-core-test-collect-rewrites-errors-on-nonzero-exit ()
  "Rewrite collection must surface ast-grep command failures."
  (let ((ast-grep-executable "sh"))
    (should-error
     (ast-grep--collect-rewrites "console.log($A)" "logger.info($A)"
                                 ast-grep-test--fixtures-dir)
     :type 'error)))

(ert-deftest ast-grep-core-test-end-to-end-sync ()
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

(ert-deftest ast-grep-core-test-end-to-end-async-parser-matches-sync ()
  "Async per-line parser must produce the same candidates as the sync parser."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((output (ast-grep--run-command "console.log($A)"
                                        ast-grep-test--fixtures-dir))
         (sync (ast-grep--parse-stream-output output))
         (async (delq nil
                      (mapcar #'ast-grep--parse-stream-line
                              (split-string output "\n" t)))))
    (should (equal sync async))))

(ert-deftest ast-grep-core-test-end-to-end-goto-from-candidate ()
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

(ert-deftest ast-grep-core-test-end-to-end-collect-rewrites ()
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

(provide 'ast-grep-core-test)

;;; ast-grep-core-test.el ends here
