;;; ast-grep-consult-test.el --- Consult backend tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-consult)

(ert-deftest ast-grep-consult-test-async-builder-respects-min-input ()
  "Builder returns nil when input is shorter than `ast-grep-async-min-input'."
  (let ((ast-grep-async-min-input 3)
        (ast-grep-executable "ast-grep"))
    (should-not (ast-grep--async-builder "" "/dir"))
    (should-not (ast-grep--async-builder "ab" "/dir"))
    (should (equal (ast-grep--async-builder "abc" "/dir")
                   '("ast-grep" "run" "--pattern=abc" "--json=stream" "/dir")))))

(ert-deftest ast-grep-consult-test-consult-preview-uses-character-columns ()
  "Consult preview must share the same character-column navigation."
  (with-temp-buffer
    (insert "\tconsole.log(x);\n")
    (let ((candidate
           (ast-grep--format-candidate
            (list :file "ignored.js"
                  :start-line 0
                  :start-column 1
                  :text "console.log(x)"))))
      (cl-letf (((symbol-function 'consult--file-preview)
                 (lambda ()
                   (lambda (&rest _args)
                     (current-buffer))))
                ((symbol-function 'pulse-momentary-highlight-one-line)
                 (lambda (&rest _) nil)))
        (funcall (ast-grep--state) 'preview candidate)
        (should (= (point) 2))
        (should (looking-at-p "console\\.log(x)"))))))

(ert-deftest ast-grep-consult-test-consult-state-forwards-cleanup-to-preview ()
  "Consult cleanup actions must reach the underlying file preview state."
  (let (calls)
    (cl-letf (((symbol-function 'consult--file-preview)
               (lambda ()
                 (lambda (action cand)
                   (push (list action cand) calls)
                   'preview-result))))
      (should (eq (funcall (ast-grep--state) 'cleanup nil)
                  'preview-result))
      (should (equal calls '((cleanup nil)))))))

(ert-deftest ast-grep-consult-test-consult-state-return-does-not-jump ()
  "Final navigation is owned by `ast-grep--search-consult', not state return."
  (let* ((candidate (ast-grep--format-candidate
                     (list :file "ignored.js"
                           :start-line 0
                           :start-column 1
                           :text "console.log(x)")))
         (goto-calls 0)
         preview-calls)
    (cl-letf (((symbol-function 'consult--file-preview)
               (lambda ()
                 (lambda (action cand)
                   (push (list action cand) preview-calls)
                   'preview-result)))
              ((symbol-function 'ast-grep--goto-line-column)
               (lambda (&rest _)
                 (cl-incf goto-calls))))
      (should (eq (funcall (ast-grep--state) 'return candidate)
                  'preview-result))
      (should (zerop goto-calls))
      (should (equal preview-calls '((return "ignored.js")))))))

(ert-deftest ast-grep-consult-test-end-to-end-async-builder-produces-runnable-command ()
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

(ert-deftest ast-grep-consult-test-end-to-end-async-pipeline ()
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

(ert-deftest ast-grep-consult-test-end-to-end-async-pipeline-min-input ()
  "Pipeline stays quiet when input is below the min-input threshold."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (skip-unless ast-grep-test--consult-available)
  (let* ((ast-grep-async-min-input 3)
         (source (ast-grep--async-source ast-grep-test--fixtures-dir))
         ;; Drive with a short pattern; the builder gates spawning,
         ;; so no subprocess starts and no candidates arrive.
         (candidates (ast-grep-test--drive-async source "ab" 1.0)))
    (should (null candidates))))

(ert-deftest ast-grep-consult-test-search-with-consult-wires-goto-match ()
  "With consult available `ast-grep--search-consult' should hand the selected
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
            (ast-grep--search-consult ast-grep-test--fixtures-dir)
            (should (= 1 calls))
            (should (equal (buffer-file-name) target))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2))
            (should (looking-at-p "console\\.log(name)")))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting target)))
            (kill-buffer buf)))))))
(provide 'ast-grep-consult-test)

;;; ast-grep-consult-test.el ends here
