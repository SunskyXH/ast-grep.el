;;; ast-grep-entry-test.el --- Entry point tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for ast-grep.el.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep)

(ert-deftest ast-grep-entry-test-select-backend-sync ()
  "`sync' backend is honoured regardless of async backend availability."
  (let ((ast-grep-search-backend 'sync))
    (should (eq 'sync (ast-grep--select-backend)))))

(ert-deftest ast-grep-entry-test-select-backend-auto-with-ivy-no-counsel ()
  "Auto falls back to sync when ivy-mode is on but counsel is missing."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode t))
    (cl-letf (((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () nil))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-auto-without-consult ()
  "Auto backend falls back to sync when no async backend is available."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode nil)
        (helm-mode nil))
    (cl-letf (((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () nil))
              ((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () nil))
              ((symbol-function 'ast-grep--helm-backend-available-p)
               (lambda () nil)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-auto-with-consult ()
  "Auto backend picks consult when it is loadable and UI modes are off."
  (skip-unless ast-grep-test--consult-available)
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode nil)
        (helm-mode nil))
    (should (eq 'consult (ast-grep--select-backend)))))

(ert-deftest ast-grep-entry-test-select-backend-auto-with-ivy-and-counsel ()
  "Auto backend picks ivy when ivy-mode is active and counsel is available."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode t))
    (cl-letf (((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t)))
      (should (eq 'ivy (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-auto-with-helm ()
  "Auto backend picks Helm when `helm-mode' is active and Helm is available."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode nil)
        (helm-mode t))
    (cl-letf (((symbol-function 'ast-grep--helm-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t)))
      (should (eq 'helm (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-auto-with-helm-no-helm ()
  "Auto falls back to sync when `helm-mode' is on but Helm is missing."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode nil)
        (helm-mode t))
    (cl-letf (((symbol-function 'ast-grep--helm-backend-available-p)
               (lambda () nil))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-force-consult-without-consult ()
  "Forced `consult' falls back to sync when consult is not loadable."
  (let ((ast-grep-search-backend 'consult))
    (cl-letf (((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () nil)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-force-ivy-without-counsel ()
  "Forced `ivy' falls back to sync when counsel/ivy are not loadable."
  (let ((ast-grep-search-backend 'ivy))
    (cl-letf (((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () nil)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-select-backend-force-helm-without-helm ()
  "Forced `helm' falls back to sync when Helm is not loadable."
  (let ((ast-grep-search-backend 'helm))
    (cl-letf (((symbol-function 'ast-grep--helm-backend-available-p)
               (lambda () nil)))
      (should (eq 'sync (ast-grep--select-backend))))))

(ert-deftest ast-grep-entry-test-backend-description-auto-resolves-backend ()
  "`ast-grep--backend-description' reports auto and the resolved backend."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode t))
    (cl-letf (((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t)))
      (should (equal (ast-grep--backend-description)
                     "ast-grep backend: auto -> ivy")))))

(ert-deftest ast-grep-entry-test-backend-description-forced-fallback ()
  "`ast-grep--backend-description' reports forced backends that fall back."
  (let ((ast-grep-search-backend 'consult))
    (cl-letf (((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () nil)))
      (should (equal (ast-grep--backend-description)
                     "ast-grep backend: consult -> sync")))))

(ert-deftest ast-grep-entry-test-describe-backend-message ()
  "`ast-grep-describe-backend' messages and returns the backend description."
  (let ((ast-grep-search-backend 'sync)
        captured-message)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq captured-message (apply #'format format-string args)))))
      (should (equal (ast-grep-describe-backend)
                     "ast-grep backend: sync"))
      (should (equal captured-message
                     "ast-grep backend: sync")))))

(ert-deftest ast-grep-entry-test-search-dispatch-does-not-route-returned-candidate ()
  "`ast-grep-search' only dispatches; backends own candidate actions."
  (let ((ast-grep-search-backend 'sync)
        (backend-calls 0)
        (goto-calls 0))
    (cl-letf (((symbol-function 'ast-grep--executable-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--search-sync)
               (lambda (_directory)
                 (cl-incf backend-calls)
                 "test.js:1:0:hit"))
              ((symbol-function 'ast-grep--goto-match)
               (lambda (_candidate)
                 (cl-incf goto-calls))))
      (ast-grep-search ast-grep-test--fixtures-dir)
      (should (= 1 backend-calls))
      (should (zerop goto-calls)))))

(ert-deftest ast-grep-entry-test-search-with-ivy-mode-dispatches-ivy ()
  "With ivy-mode active and ivy available, route to ivy, not consult."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode t)
        backend-called
        directory-called)
    (cl-letf (((symbol-function 'ast-grep--executable-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--ivy-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--run-search-backend)
               (lambda (backend directory)
                 (setq backend-called backend
                       directory-called directory))))
      (ast-grep-search ast-grep-test--fixtures-dir)
      (should (eq backend-called 'ivy))
      (should (equal directory-called ast-grep-test--fixtures-dir)))))

(ert-deftest ast-grep-entry-test-search-with-helm-mode-dispatches-helm ()
  "With Helm mode active and Helm available, route to Helm, not consult."
  (let ((ast-grep-search-backend 'auto)
        (ivy-mode nil)
        (helm-mode t)
        backend-called
        directory-called)
    (cl-letf (((symbol-function 'ast-grep--executable-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--helm-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--consult-backend-available-p)
               (lambda () t))
              ((symbol-function 'ast-grep--run-search-backend)
               (lambda (backend directory)
                 (setq backend-called backend
                       directory-called directory))))
      (ast-grep-search ast-grep-test--fixtures-dir)
      (should (eq backend-called 'helm))
      (should (equal directory-called ast-grep-test--fixtures-dir)))))

(ert-deftest ast-grep-entry-test-rewrite-applies-yes-to-all ()
  "Driving the prompt with `!' rewrites every match in the buffer.
The file on disk stays untouched; the user is expected to save."
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
          ;; Buffer should be modified (unsaved), matching
          ;; `project-query-replace-regexp' semantics.
          (with-current-buffer buf
            (should (buffer-modified-p))
            (let ((content (buffer-string)))
              (should (string-match-p "logger\\.info(\"hello\")" content))
              (should (string-match-p "logger\\.info(x)" content))
              (should (string-match-p "logger\\.info(name)" content))
              (should-not (string-match-p "console\\.log" content))))
          ;; File on disk should be untouched until the user saves.
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (should (string-match-p "console\\.log(\"hello\")"
                                    (buffer-string)))))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tmp-dir t))))

(ert-deftest ast-grep-entry-test-rewrite-quit-skips-remaining ()
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

(ert-deftest ast-grep-entry-test-rewrite-skips-stale-matches ()
  "Stale or invalid ranges are skipped, including changed nested matches."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (dolist (case '((prefix "foo(x);\n" "prefix\nfoo(x);\n"
                         "foo($A)" "bar($A)" "prefix\nfoo(x);\n" 0 1 0)
                  (shortened "foo(x);\n" "x"
                             "foo($A)" "bar($A)" "x" 0 1 0)
                  (column-overflow "       foo(x);\n" "a\nb\nc\n foo(x);\n"
                                   "foo($A)" "bar($A)" "a\nb\nc\n foo(x);\n" 0 1 0)
                  (nested-shorter "f(g(x)); // tail\n" "f(g(x)); // tail\n"
                                  "$F($$$A)" "X" "f(X); // tail\n" 1 1 1)
                  (nested-same-length "f(g(x)); // tail\n" "f(g(x)); // tail\n"
                                      "$F($$$A)" "h(x)" "f(h(x)); // tail\n" 1 1 1)
                  (continue-all "a(x);\nf(g(x)); // tail\n"
                                "a(x);\nf(g(x)); // tail\n"
                                "$F($$$A)" "X" "X;\nf(X); // tail\n" 2 1 1)
                  (prompt-edit "foo(x);\n" "foo(x);\n"
                               "foo($A)" "bar($A)" "prefix\nfoo(x);\n" 0 1 1)
                  (crlf "function f() {\r\n  foo();\r\n  bar();\r\n}\r\n"
                        "function f() {\n  foo();\n  bar();\n}\n"
                        "function $F() { $$$B }" "function $F() {\n  $$$B\n}"
                        "function f() {\n  foo();\n  bar();\n}\n" 1 0 1)))
    (pcase-let* ((`(,name ,disk ,current ,pattern ,replacement ,expected
                         ,replaced ,skipped ,expected-prompts) case)
                 (tmp-dir (make-temp-file "ast-grep-stale-" t))
                 (tmp-file (expand-file-name "sample.js" tmp-dir))
                 (prompts 0)
                 (summary nil)
                 (buf nil))
      (ert-info ((format "Rewrite case: %s" name))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region disk nil tmp-file nil 'silent))
              (setq buf (find-file-noselect tmp-file))
              (with-current-buffer buf
                (erase-buffer)
                (insert current))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (prompt &rest _)
                           (if (string-prefix-p "ast-grep pattern" prompt)
                               pattern replacement)))
                        ((symbol-function 'read-char-choice)
                         (lambda (&rest _)
                           (cl-incf prompts)
                           (when (eq name 'prompt-edit)
                             (goto-char (point-min))
                             (insert "prefix\n"))
                           ?!))
                        ((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _) buffer))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq summary (apply #'format format-string args)))))
                (ast-grep-rewrite tmp-dir))
              (with-current-buffer buf
                (should (equal (buffer-string) expected)))
              (should (= prompts expected-prompts))
              (should (string-prefix-p
                       (format "Replaced %d match(es) in %d file(s); skipped %d"
                               replaced (min replaced 1) skipped)
                       summary))
              (with-temp-buffer
                (insert-file-contents-literally tmp-file)
                (should (equal (buffer-string) disk))))
          (when (buffer-live-p buf)
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf))
          (delete-directory tmp-dir t))))))

(ert-deftest ast-grep-entry-test-rewrite-preserves-narrowing ()
  "Rewrite uses whole-file coordinates and restores an existing restriction."
  (skip-unless (ast-grep-test--ast-grep-available-p))
  (let* ((tmp-dir (make-temp-file "ast-grep-narrowed-" t))
         (tmp-file (expand-file-name "sample.js" tmp-dir))
         (disk "foo(x);\nkeep_y;\n")
         buf)
    (unwind-protect
        (progn
          (write-region disk nil tmp-file nil 'silent)
          (setq buf (find-file-noselect tmp-file))
          (with-current-buffer buf
            (goto-char (point-min))
            (forward-line 1)
            (narrow-to-region (point) (point-max)))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (if (string-prefix-p "ast-grep pattern" prompt)
                           "foo($A)" "logger.info($A)")))
                    ((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?!))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) buffer)))
            (ast-grep-rewrite tmp-dir))
          (with-current-buffer buf
            (should (buffer-narrowed-p))
            (should (equal (buffer-string) "keep_y;\n"))
            (should (= (point-min) (1+ (length "logger.info(x);\n"))))
            (save-restriction
              (widen)
              (should (equal (buffer-string) "logger.info(x);\nkeep_y;\n"))))
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (should (equal (buffer-string) disk))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tmp-dir t))))

(provide 'ast-grep-entry-test)

;;; ast-grep-entry-test.el ends here
