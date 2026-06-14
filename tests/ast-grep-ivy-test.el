;;; ast-grep-ivy-test.el --- Ivy backend tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-ivy)

(ert-deftest ast-grep-ivy-test-ivy-more-chars-respects-min-input ()
  "The ivy placeholder uses `ast-grep-async-min-input'."
  (let ((ast-grep-async-min-input 4))
    (should (equal (ast-grep--ivy-more-chars "ab")
                   '("" "2 chars more")))
    (should-not (ast-grep--ivy-more-chars "abcd"))))

(ert-deftest ast-grep-ivy-test-ivy-collection-starts-shell-command ()
  "The ivy adapter uses a shell-quoted command string for old counsel versions."
  (let ((ast-grep-async-min-input 3)
        (ast-grep-executable "ast-grep")
        captured-command
        captured-sentinel
        captured-filter
        captured-name)
    (cl-letf (((symbol-function 'counsel--async-command)
               (lambda (command sentinel filter &optional name)
                 (setq captured-command command
                       captured-sentinel sentinel
                       captured-filter filter
                       captured-name name)))
              ((symbol-function 'counsel-delete-process)
               (lambda (&rest _))))
      (funcall (ast-grep--ivy-collection "/dir") "abc")
      (should (stringp captured-command))
      (should (equal captured-command
                     (ast-grep--command-shell-string
                      '("ast-grep" "run" "--pattern=abc"
                        "--json=stream" "/dir"))))
      (should-not captured-sentinel)
      (should (functionp captured-filter))
      (should-not captured-name))))

(ert-deftest ast-grep-ivy-test-ivy-available-in-backend-sandbox ()
  "Ivy backend sandboxes must load real ivy and counsel."
  (skip-unless (member (getenv "AST_GREP_TEST_BACKEND") '("ivy" "full")))
  (should ast-grep-test--ivy-available)
  (should (ast-grep--ivy-available-p)))

(ert-deftest ast-grep-ivy-test-real-counsel-async-command-contract ()
  "The ivy sandbox exercises real counsel async filtering and cleanup."
  (skip-unless ast-grep-test--ivy-available)
  (let* ((fake-executable
          (make-temp-file
           "ast-grep-fake-" nil nil
           (concat
            "#!/bin/sh\n"
            "printf '%s\\n' '{\"file\":\"fake.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x:y\"}'\n"
            "sleep 1\n")))
         (ast-grep-async-min-input 3)
         (ast-grep-executable fake-executable)
         (counsel-async-command-delay 0)
         (ast-grep--ivy-generation 0)
         (display "fake.js:1:0:x:y")
         proc
         match)
    (set-file-modes fake-executable #o700)
    (unwind-protect
        (progn
          (ast-grep--reset-candidate-table)
          (funcall (ast-grep--ivy-collection default-directory) "abc")
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (not (setq proc (get-process ast-grep--ivy-process-name)))
                        (< (float-time) deadline))
              (accept-process-output nil 0.02))
            (should (processp proc))
            (should (equal (process-name proc) ast-grep--ivy-process-name)))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (not (setq match (gethash display
                                                   ast-grep--candidate-table)))
                        (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should match)
          (should (eq (ast-grep--candidate-match display) match))
          (should (equal (plist-get match :text) "x:y"))
          (ast-grep--ivy-stop-process)
          (should-not (get-process ast-grep--ivy-process-name)))
      (ast-grep--ivy-stop-process)
      (when (file-exists-p fake-executable)
        (delete-file fake-executable)))))

(ert-deftest ast-grep-ivy-test-search-wires-action-and-unwind ()
  "`ast-grep--search-ivy' wires candidate action and process cleanup."
  (let* ((selection (ast-grep--format-candidate
                     (list :file ast-grep-test--sample-file
                           :start-line 4
                           :start-column 2
                           :text "console.log(name)")))
        (ivy-calls 0)
        ivy-action
        ivy-update-fn
        ivy-unwind
        (buffer-before (find-buffer-visiting ast-grep-test--sample-file))
        (pulse-calls 0)
        (stopped 0))
    (cl-letf (((symbol-function 'ivy-read)
               (lambda (&rest args)
                 (cl-incf ivy-calls)
                 (let ((plist (cddr args)))
                   (setq ivy-action (plist-get plist :action)
                         ivy-update-fn (plist-get plist :update-fn)
                         ivy-unwind (plist-get plist :unwind))
                   (funcall ivy-action selection))))
              ((symbol-function 'ast-grep--ivy-stop-process)
               (lambda () (cl-incf stopped)))
              ((symbol-function 'pulse-momentary-highlight-one-line)
               (lambda (&rest _) (cl-incf pulse-calls))))
      (unwind-protect
          (progn
            (ast-grep--search-ivy ast-grep-test--fixtures-dir)
            (should (= 1 ivy-calls))
            (should (functionp ivy-action))
            (should (eq ivy-update-fn 'auto))
            (should (functionp ivy-unwind))
            (funcall ivy-unwind)
            (should (= 1 stopped))
            (should (equal (buffer-file-name) ast-grep-test--sample-file))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2))
            (should (= pulse-calls 1)))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting ast-grep-test--sample-file)))
            (kill-buffer buf)))))))

(ert-deftest ast-grep-ivy-test-ivy-action-ignores-non-candidates ()
  "`ast-grep--ivy-action' ignores placeholder text and unmatched strings."
  (let ((find-file-calls 0)
        (pulse-calls 0))
    (cl-letf (((symbol-function 'find-file)
               (lambda (&rest _) (cl-incf find-file-calls)))
              ((symbol-function 'pulse-momentary-highlight-one-line)
               (lambda (&rest _) (cl-incf pulse-calls))))
      (should-not (ast-grep--ivy-action "1 chars more"))
      (should-not (ast-grep--ivy-action "not a candidate"))
      (should (zerop find-file-calls))
      (should (zerop pulse-calls)))))

(ert-deftest ast-grep-ivy-test-ivy-collection-short-input-stops-process ()
  "Short ivy input clears stale matches and stops the previous process."
  (let ((ast-grep-async-min-input 3)
        delete-called
        deleted-name
        (candidate (ast-grep--format-candidate
                    (list :file "stale.js"
                          :start-line 0
                          :start-column 0
                          :text "stale"))))
    (should (ast-grep--candidate-match candidate))
    (cl-letf (((symbol-function 'counsel-delete-process)
               (lambda (&optional name)
                 (setq delete-called t
                       deleted-name name))))
      (should (equal (funcall (ast-grep--ivy-collection "/dir") "ab")
                     '("" "1 chars more")))
      (should delete-called)
      (should-not deleted-name)
      (should-not (gethash (substring-no-properties candidate)
                           ast-grep--candidate-table)))))

(ert-deftest ast-grep-ivy-test-ivy-collection-short-input-cancels-pending-timer ()
  "Short ivy input cancels counsel's delayed async command timer."
  (let ((ast-grep-async-min-input 3)
        (timer (run-with-timer 3600 nil #'ignore))
        delete-called
        deleted-name)
    (unwind-protect
        (let ((counsel--async-timer timer))
          (cl-letf (((symbol-function 'counsel-delete-process)
                     (lambda (&optional name)
                       (setq delete-called t
                             deleted-name name))))
            (should (equal (funcall (ast-grep--ivy-collection "/dir") "ab")
                           '("" "1 chars more")))
            (should delete-called)
            (should-not deleted-name)
            (should-not counsel--async-timer)))
      (when (timerp timer)
        (cancel-timer timer)))))

(ert-deftest ast-grep-ivy-test-ivy-async-filter-transforms-json-to-display ()
  "The ivy async process filter registers matches and forwards display lines."
  (let* ((proc (start-process "ast-grep-test-filter" nil "true"))
         (captured ""))
    (unwind-protect
        (cl-letf (((symbol-function 'counsel--async-filter)
                   (lambda (_process str)
                     (setq captured (concat captured str)))))
          (ast-grep--reset-candidate-table)
          (ast-grep--ivy-async-filter
           proc
           (concat "{\"file\":\"a:b.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x:y\"}\n"
                   "{\"file\":\"b.js\",\"range\":{\"start\":{\"line\":2,\"column\":4}},\"text\":\"y\"}\n"))
          (should (equal captured "a:b.js:1:0:x:y\nb.js:3:4:y\n"))
          (let* ((display (car (split-string captured "\n" t)))
                 (match (ast-grep--candidate-match
                         (substring-no-properties display))))
            (should (equal (plist-get match :file) "a:b.js"))
            (should (= (plist-get match :start-line) 0))
            (should (= (plist-get match :start-column) 0))
            (should (equal (plist-get match :text) "x:y"))))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest ast-grep-ivy-test-ivy-async-filter-ignores-stale-generation ()
  "Output from an old ivy process must not repopulate current candidates."
  (let* ((proc (start-process ast-grep--ivy-process-name nil "true"))
         (ast-grep--ivy-generation 2)
         (captured "")
         (line "{\"file\":\"stale.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"stale\"}\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'counsel--async-filter)
                   (lambda (_process str)
                     (setq captured (concat captured str)))))
          (ast-grep--reset-candidate-table)
          (ast-grep--ivy-async-filter proc line 1)
          (should (equal captured ""))
          (should-not (gethash "stale.js:1:0:stale"
                               ast-grep--candidate-table)))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest ast-grep-ivy-test-ivy-async-filter-buffers-partial-lines ()
  "Partial JSON chunks are buffered until a newline arrives."
  (let* ((proc (start-process "ast-grep-test-filter" nil "true"))
         (captured ""))
    (unwind-protect
        (cl-letf (((symbol-function 'counsel--async-filter)
                   (lambda (_process str)
                     (setq captured (concat captured str)))))
          (ast-grep--ivy-async-filter
           proc
           "{\"file\":\"a.js\",\"range\":{\"start\":{\"line\":0,\"")
          (should (equal captured ""))
          (ast-grep--ivy-async-filter
           proc
           "column\":0}},\"text\":\"x\"}\n")
          (should (equal captured "a.js:1:0:x\n")))
      (when (process-live-p proc) (delete-process proc)))))
(provide 'ast-grep-ivy-test)

;;; ast-grep-ivy-test.el ends here
