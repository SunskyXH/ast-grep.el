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
               (lambda (command sentinel filter name)
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
      (should (equal captured-name ast-grep--ivy-process-name)))))

(ert-deftest ast-grep-ivy-test-search-wires-action-and-unwind ()
  "`ast-grep--search-ivy' wires candidate action and process cleanup."
  (let* ((selection (ast-grep--format-candidate
                     (list :file ast-grep-test--sample-file
                           :start-line 4
                           :start-column 2
                           :text "console.log(name)")))
         (ivy-calls 0)
         ivy-action
         ivy-unwind
         (buffer-before (find-buffer-visiting ast-grep-test--sample-file))
         (stopped 0))
    (cl-letf (((symbol-function 'ivy-read)
               (lambda (&rest args)
                 (cl-incf ivy-calls)
                 (let ((plist (cddr args)))
                   (setq ivy-action (plist-get plist :action)
                         ivy-unwind (plist-get plist :unwind))
                   (funcall ivy-action selection))))
              ((symbol-function 'ast-grep--ivy-stop-process)
               (lambda () (cl-incf stopped))))
      (unwind-protect
          (progn
            (ast-grep--search-ivy ast-grep-test--fixtures-dir)
            (should (= 1 ivy-calls))
            (should (functionp ivy-action))
            (should (functionp ivy-unwind))
            (funcall ivy-unwind)
            (should (= 1 stopped))
            (should (equal (buffer-file-name) ast-grep-test--sample-file))
            (should (= (line-number-at-pos) 5))
            (should (= (current-column) 2)))
        (unless buffer-before
          (when-let ((buf (find-buffer-visiting ast-grep-test--sample-file)))
            (kill-buffer buf)))))))

(ert-deftest ast-grep-ivy-test-ivy-collection-short-input-stops-process ()
  "Short ivy input clears stale matches and stops the previous process."
  (let ((ast-grep-async-min-input 3)
        deleted-name
        (candidate (ast-grep--format-candidate
                    (list :file "stale.js"
                          :start-line 0
                          :start-column 0
                          :text "stale"))))
    (should (ast-grep--candidate-match candidate))
    (cl-letf (((symbol-function 'counsel-delete-process)
               (lambda (name) (setq deleted-name name))))
      (should (equal (funcall (ast-grep--ivy-collection "/dir") "ab")
                     '("" "1 chars more")))
      (should (equal deleted-name ast-grep--ivy-process-name))
      (should-not (gethash (substring-no-properties candidate)
                           ast-grep--candidate-table)))))

(ert-deftest ast-grep-ivy-test-ivy-collection-short-input-cancels-pending-timer ()
  "Short ivy input cancels counsel's delayed async command timer."
  (let ((ast-grep-async-min-input 3)
        (timer (run-with-timer 3600 nil #'ignore))
        deleted-name)
    (unwind-protect
        (let ((counsel--async-timer timer))
          (cl-letf (((symbol-function 'counsel-delete-process)
                     (lambda (name) (setq deleted-name name))))
            (should (equal (funcall (ast-grep--ivy-collection "/dir") "ab")
                           '("" "1 chars more")))
            (should (equal deleted-name ast-grep--ivy-process-name))
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
          (ast-grep--ivy-async-filter
           proc
           (concat "{\"file\":\"a:b.js\",\"range\":{\"start\":{\"line\":0,\"column\":0}},\"text\":\"x:y\"}\n"
                   "{\"file\":\"b.js\",\"range\":{\"start\":{\"line\":2,\"column\":4}},\"text\":\"y\"}\n"))
          (should (equal captured "a:b.js:1:0:x:y\nb.js:3:4:y\n"))
          (let ((match (ast-grep--candidate-match "a:b.js:1:0:x:y")))
            (should (equal (plist-get match :file) "a:b.js"))
            (should (= (plist-get match :start-line) 0))
            (should (= (plist-get match :start-column) 0))))
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
