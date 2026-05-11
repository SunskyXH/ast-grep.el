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

(provide 'test-ast-grep)

;;; ast-grep-test.el ends here
