;;; ast-grep-outline-test.el --- Outline/imenu tests for ast-grep.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the ast-grep outline imenu integration.

;;; Code:

(require 'ast-grep-test-helper)
(require 'ast-grep-outline)

(defconst ast-grep-outline-test--sample-ts
  (expand-file-name "sample.ts" ast-grep-test--fixtures-dir)
  "TypeScript fixture exercised by the real outline binary.")

(defconst ast-grep-outline-test--stream
  (concat
   "{\"path\":\"x.ts\",\"language\":\"TypeScript\",\"items\":["
   "{\"symbolType\":\"class\",\"name\":\"Widget\","
   "\"range\":{\"start\":{\"line\":0,\"column\":0}},\"members\":["
   "{\"symbolType\":\"field\",\"name\":\"id\","
   "\"range\":{\"start\":{\"line\":1,\"column\":2}}},"
   "{\"symbolType\":\"method\",\"name\":\"render\","
   "\"range\":{\"start\":{\"line\":2,\"column\":2}}}]},"
   "{\"symbolType\":\"function\",\"name\":\"greet\","
   "\"range\":{\"start\":{\"line\":4,\"column\":0}}},"
   "{\"symbolType\":\"constant\",\"name\":\"K\","
   "\"range\":{\"start\":{\"line\":5,\"column\":0}}}]}")
  "Hand-built outline stream matching `ast-grep-outline-test--insert-source'.")

(defun ast-grep-outline-test--insert-source ()
  "Insert source text whose lines/columns line up with the fixture stream."
  (insert "class Widget {\n"
          "  id;\n"
          "  render() {}\n"
          "}\n"
          "function greet() {}\n"
          "const K = 1;\n"))

(defun ast-grep-outline-test--pos (line column)
  "Return the buffer position for 0-based LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (forward-char column)
    (point)))

(ert-deftest ast-grep-outline-test-build-command ()
  "The outline command targets a single file with stream JSON."
  (let ((ast-grep-executable "ast-grep"))
    (should (equal (ast-grep--build-outline-command "/tmp/a.ts")
                   '("ast-grep" "outline" "--json=stream" "/tmp/a.ts")))))

(ert-deftest ast-grep-outline-test-group-title ()
  "Known symbol types map to titles; unknown types fall back to capitalize."
  (should (equal (ast-grep--outline-group-title "class") "Classes"))
  (should (equal (ast-grep--outline-group-title "method") "Methods"))
  (should (equal (ast-grep--outline-group-title "widget") "Widget")))

(ert-deftest ast-grep-outline-test-parse ()
  "Parsing collects items and tolerates blank and malformed lines."
  (let* ((output (concat ast-grep-outline-test--stream "\nnot-json\n"))
         (items (ast-grep--outline-parse output)))
    (should (= (length items) 3))
    (should (equal (plist-get (car items) :name) "Widget"))
    (should (= (length (plist-get (car items) :members)) 2)))
  (should-not (ast-grep--outline-parse nil))
  (should-not (ast-grep--outline-parse ""))
  (should-not (ast-grep--outline-parse "not-json")))

(ert-deftest ast-grep-outline-test-index-structure ()
  "The imenu index groups by type, qualifies members, and keeps source order."
  (with-temp-buffer
    (ast-grep-outline-test--insert-source)
    (cl-letf (((symbol-function 'ast-grep--run-outline)
               (lambda (_file) ast-grep-outline-test--stream)))
      (setq buffer-file-name "x.ts")
      (let ((index (ast-grep--outline-imenu-index)))
        ;; Groups appear in canonical order, not source order.
        (should (equal (mapcar #'car index)
                       '("Classes" "Functions" "Methods" "Fields" "Constants")))
        (should (equal (cdr (assoc "Classes" index))
                       (list (cons "Widget" (ast-grep-outline-test--pos 0 0)))))
        (should (equal (cdr (assoc "Methods" index))
                       (list (cons "Widget.render"
                                   (ast-grep-outline-test--pos 2 2)))))
        (should (equal (cdr (assoc "Fields" index))
                       (list (cons "Widget.id"
                                   (ast-grep-outline-test--pos 1 2)))))
        (should (equal (cdr (assoc "Functions" index))
                       (list (cons "greet" (ast-grep-outline-test--pos 4 0)))))))))

(ert-deftest ast-grep-outline-test-index-requires-file ()
  "A buffer with no file yields no index rather than an error."
  (with-temp-buffer
    (insert "class A {}")
    (should-not buffer-file-name)
    (should-not (ast-grep--outline-imenu-index))))

(ert-deftest ast-grep-outline-test-mode-installs-and-restores-default ()
  "Enabling sets the local index function; disabling restores the default."
  (with-temp-buffer
    (should-not (local-variable-p 'imenu-create-index-function))
    (ast-grep-outline-mode 1)
    (should (eq imenu-create-index-function #'ast-grep--outline-imenu-index))
    (should (local-variable-p 'imenu-create-index-function))
    (ast-grep-outline-mode -1)
    (should-not (local-variable-p 'imenu-create-index-function))))

(ert-deftest ast-grep-outline-test-mode-restores-prior-local ()
  "Disabling restores a pre-existing buffer-local index function."
  (with-temp-buffer
    (setq-local imenu-create-index-function #'ignore)
    (ast-grep-outline-mode 1)
    (should (eq imenu-create-index-function #'ast-grep--outline-imenu-index))
    (ast-grep-outline-mode -1)
    (should (eq imenu-create-index-function #'ignore))))

(ert-deftest ast-grep-outline-test-command-requires-file ()
  "`ast-grep-outline' refuses to run outside a file-visiting buffer."
  (with-temp-buffer
    (cl-letf (((symbol-function 'ast-grep--executable-available-p)
               (lambda () t)))
      (should-error (ast-grep-outline) :type 'user-error))))

(ert-deftest ast-grep-outline-test-command-dispatches-picker ()
  "`ast-grep-outline' dispatches to a picker and restores imenu state.
Both pickers are stubbed so the test is agnostic to whether consult is
installed in the current sandbox."
  ;; Load consult-imenu first (when available) so the require inside
  ;; `ast-grep-outline' is a no-op and does not clobber the stub below.
  (require 'consult-imenu nil t)
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (setq-local imenu--index-alist 'sentinel)
    (let ((called nil) (ivy-mode nil))
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'imenu)
                 (lambda (&rest _) (interactive) (setq called 'imenu)))
                ((symbol-function 'consult-imenu)
                 (lambda (&rest _) (interactive) (setq called 'consult))))
        (ast-grep-outline)
        (should (memq called '(imenu consult)))
        ;; The one-shot leaves the buffer's imenu configuration untouched.
        (should-not (local-variable-p 'imenu-create-index-function))
        (should (eq imenu--index-alist 'sentinel))))))

(ert-deftest ast-grep-outline-test-command-cleanup-pins-origin-buffer ()
  "Cleanup restores the origin buffer even when the picker switches buffers.
Some picker exits leave another buffer current when they return (e.g.
helm-imenu's quit-and-find-file on \\`C-x C-f'); the unwind cleanup must
still target the buffer whose imenu state was swapped, not whichever
buffer the action landed in."
  (require 'consult-imenu nil t)
  (let ((origin (generate-new-buffer " *sg-outline-origin*"))
        (other (generate-new-buffer " *sg-outline-other*")))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq buffer-file-name "x.ts")
            (setq-local imenu--index-alist 'sentinel)
            (let ((ivy-mode nil))
              (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                         (lambda () t))
                        ((symbol-function 'imenu)
                         (lambda (&rest _) (interactive) (set-buffer other)))
                        ((symbol-function 'consult-imenu)
                         (lambda (&rest _) (interactive) (set-buffer other))))
                (ast-grep-outline))))
          ;; The origin buffer got its imenu state back...
          (with-current-buffer origin
            (should-not (local-variable-p 'imenu-create-index-function))
            (should (eq imenu--index-alist 'sentinel)))
          ;; ...and the buffer the picker landed in was left alone.
          (with-current-buffer other
            (should-not (local-variable-p 'imenu-create-index-function))
            (should-not (local-variable-p 'imenu--index-alist))))
      (kill-buffer origin)
      (kill-buffer other))))

(ert-deftest ast-grep-outline-test-real-binary-index ()
  "The real outline binary produces the expected index for sample.ts."
  (skip-unless (ast-grep-test--outline-available-p))
  (let ((buffer (find-file-noselect ast-grep-outline-test--sample-ts)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((index (ast-grep--outline-imenu-index)))
            (should (equal (mapcar #'car index)
                           '("Classes" "Interfaces" "Functions"
                             "Methods" "Constructors" "Fields" "Constants")))
            (should (equal (mapcar #'car (cdr (assoc "Fields" index)))
                           '("Point.x" "Point.y" "Widget.id")))
            (should (equal (mapcar #'car (cdr (assoc "Classes" index)))
                           '("Widget")))
            (should (equal (mapcar #'car (cdr (assoc "Constructors" index)))
                           '("Widget.constructor")))
            ;; Positions land on the symbol they name.
            (goto-char (cdr (assoc "Widget" (cdr (assoc "Classes" index)))))
            (should (looking-at-p "export class Widget"))
            (goto-char (cdr (assoc "greet" (cdr (assoc "Functions" index)))))
            (should (looking-at-p "export function greet"))))
      (kill-buffer buffer))))

(ert-deftest ast-grep-outline-test-parse-multiple-objects ()
  "Items from every JSON object (multi-file output) are collected."
  (let* ((second (concat
                  "{\"path\":\"y.ts\",\"items\":["
                  "{\"symbolType\":\"function\",\"name\":\"farewell\","
                  "\"range\":{\"start\":{\"line\":0,\"column\":0}}}]}"))
         (output (concat ast-grep-outline-test--stream "\n" second "\nnot-json\n"))
         (items (ast-grep--outline-parse output))
         (names (mapcar (lambda (i) (plist-get i :name)) items)))
    (should (= (length items) 4))
    (should (member "Widget" names))
    (should (member "farewell" names))))

(ert-deftest ast-grep-outline-test-flatten-skips-non-string-name ()
  "Items whose name is not a string (e.g. JSON null) are skipped."
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (let* ((stream (concat
                    "{\"path\":\"x.ts\",\"items\":["
                    "{\"symbolType\":\"function\",\"name\":null,"
                    "\"range\":{\"start\":{\"line\":0,\"column\":0}}},"
                    "{\"symbolType\":\"class\",\"name\":\"Widget\","
                    "\"range\":{\"start\":{\"line\":1,\"column\":0}},\"members\":["
                    "{\"symbolType\":\"method\",\"name\":null,"
                    "\"range\":{\"start\":{\"line\":2,\"column\":0}}}]}]}"))
           (flat (ast-grep--outline-flatten
                  (ast-grep--outline-parse stream) nil)))
      (should (equal (mapcar (lambda (entry) (nth 1 entry)) flat) '("Widget"))))))

(ert-deftest ast-grep-outline-test-dedupes-duplicate-names ()
  "Same-name siblings get distinct, individually reachable labels."
  (with-temp-buffer
    (insert "x\ny\nz\n")
    (setq buffer-file-name "x.rs")
    (let* ((stream (concat
                    "{\"path\":\"x.rs\",\"items\":["
                    "{\"symbolType\":\"object\",\"name\":\"A\","
                    "\"range\":{\"start\":{\"line\":0,\"column\":0}}},"
                    "{\"symbolType\":\"object\",\"name\":\"A\","
                    "\"range\":{\"start\":{\"line\":1,\"column\":0}}}]}"))
           (index (cl-letf (((symbol-function 'ast-grep--run-outline)
                             (lambda (_file) stream)))
                    (ast-grep--outline-imenu-index)))
           (objects (cdr (assoc "Objects" index))))
      (should (equal (mapcar #'car objects) '("A" "A<2>")))
      ;; Each label resolves to its own position via `assoc'.
      (should (= (cdr (assoc "A" objects)) (ast-grep-outline-test--pos 0 0)))
      (should (= (cdr (assoc "A<2>" objects))
                 (ast-grep-outline-test--pos 1 0))))))

(ert-deftest ast-grep-outline-test-group-unknown-type-sorts-last ()
  "Groups for symbol types absent from the title map sort after known ones."
  (let ((index (ast-grep--outline-group
                '(("widget" "Gizmo" 1)
                  ("class" "Widget" 2)
                  ("function" "greet" 3)))))
    (should (equal (mapcar #'car index) '("Classes" "Functions" "Widget")))))

(ert-deftest ast-grep-outline-test-index-degrades-on-error ()
  "A failing outline run yields an empty index instead of signalling."
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((inhibit-message t))
      (cl-letf (((symbol-function 'ast-grep--run-outline)
                 (lambda (_file) (error "boom"))))
        (should-not (ast-grep--outline-imenu-index))))))

(ert-deftest ast-grep-outline-test-mode-reentrant-enable ()
  "Enabling twice then disabling still removes the buffer-local index fn."
  (with-temp-buffer
    (ast-grep-outline-mode 1)
    (ast-grep-outline-mode 1)
    (ast-grep-outline-mode -1)
    (should-not (local-variable-p 'imenu-create-index-function))))

(ert-deftest ast-grep-outline-test-command-clears-consult-cache ()
  "The one-shot busts consult-imenu's per-tick cache, then restores it."
  (skip-unless (require 'consult-imenu nil t))
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((stale (cons 0 '(("stale" . 1))))
          cache-at-dispatch)
      (setq-local consult-imenu--cache stale)
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'consult-imenu)
                 (lambda (&rest _)
                   (interactive)
                   (setq cache-at-dispatch consult-imenu--cache))))
        (ast-grep-outline)
        ;; consult saw a cleared cache, so it recomputes against our index...
        (should-not cache-at-dispatch)
        ;; ...and the buffer's prior cache is restored afterwards.
        (should (equal consult-imenu--cache stale))))))

(ert-deftest ast-grep-outline-test-mode-invalidates-imenu-cache ()
  "Toggling the mode clears a stale imenu index left by the prior source."
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (setq-local imenu--index-alist '(("old" . 1)))
    (setq-local helm-cached-imenu-alist '(("old" . 1)))
    (setq-local helm-cached-imenu-candidates '(("old" . 1)))
    (setq-local helm-cached-imenu-tick (buffer-modified-tick))
    (ast-grep-outline-mode 1)
    ;; Enabling drops the index built by the previous source.
    (should-not imenu--index-alist)
    (dolist (cache '(helm-cached-imenu-alist
                     helm-cached-imenu-candidates
                     helm-cached-imenu-tick))
      (should-not (local-variable-p cache)))
    (setq-local imenu--index-alist '(("ours" . 2)))
    (setq-local helm-cached-imenu-alist '(("ours" . 2)))
    (setq-local helm-cached-imenu-candidates '(("ours" . 2)))
    (setq-local helm-cached-imenu-tick (buffer-modified-tick))
    (ast-grep-outline-mode -1)
    ;; Disabling drops our index so the restored source rebuilds.
    (should-not imenu--index-alist)
    (dolist (cache '(helm-cached-imenu-alist
                     helm-cached-imenu-candidates
                     helm-cached-imenu-tick))
      (should-not (local-variable-p cache)))))

(ert-deftest ast-grep-outline-test-degrades-without-outline-support ()
  "An ast-grep without `outline' yields an empty index, never an error.
This keeps the rest of the package (search/rewrite) usable when the
binary predates 0.44.0 or is missing entirely."
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((inhibit-message t))
      (cl-letf (((symbol-function 'ast-grep--run-outline)
                 (lambda (_file)
                   (error "exit code 2: unrecognized subcommand 'outline'"))))
        (ast-grep-outline-mode 1)
        ;; imenu consumers call this; it returns an empty index, not an error.
        (should (eq imenu-create-index-function
                    #'ast-grep--outline-imenu-index))
        (should-not (funcall imenu-create-index-function))))))

(ert-deftest ast-grep-outline-test-command-dispatches-counsel-when-ivy ()
  "With `ivy-mode' active and counsel available, dispatch to counsel-imenu."
  (skip-unless (require 'counsel nil t))
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((ivy-mode t)
          (called nil))
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'counsel-imenu)
                 (lambda (&rest _) (interactive) (setq called 'counsel))))
        (ast-grep-outline)
        (should (eq called 'counsel))))))

(ert-deftest ast-grep-outline-test-command-ivy-without-counsel-uses-imenu ()
  "ivy-mode active but counsel missing falls to imenu, never consult.
Runs in the consult sandbox, where consult is present but counsel is not."
  (skip-unless (and (require 'consult-imenu nil t)
                    (not (require 'counsel nil t))))
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((ivy-mode t)
          (called nil))
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'consult-imenu)
                 (lambda (&rest _) (interactive) (setq called 'consult)))
                ((symbol-function 'imenu)
                 (lambda (&rest _) (interactive) (setq called 'imenu))))
        (ast-grep-outline)
        (should (eq called 'imenu))))))

(ert-deftest ast-grep-outline-test-command-dispatches-helm-when-helm ()
  "With `helm-mode' active, dispatch to helm-imenu with fresh caches."
  (skip-unless (require 'helm-imenu nil t))
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((ivy-mode nil)
          (helm-mode t)
          (called nil)
          tick-at-dispatch
          candidates-at-dispatch
          alist-at-dispatch)
      (setq-local helm-cached-imenu-alist '(("old" . 1)))
      (setq-local helm-cached-imenu-candidates '(("old" . 1)))
      (setq-local helm-cached-imenu-tick (buffer-modified-tick))
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'helm-imenu)
                 (lambda (&rest _)
                   (interactive)
                   (setq called 'helm
                         alist-at-dispatch helm-cached-imenu-alist
                         candidates-at-dispatch helm-cached-imenu-candidates
                         tick-at-dispatch helm-cached-imenu-tick)
                   ;; Simulate helm-imenu rebuilding and writing ast-grep
                   ;; candidates during this one-shot command.
                   (setq-local helm-cached-imenu-alist '(("outline" . 2)))
                   (setq-local helm-cached-imenu-candidates '(("outline" . 2)))
                   (setq-local helm-cached-imenu-tick
                               (buffer-modified-tick)))))
        (ast-grep-outline)
        (should (eq called 'helm))
        ;; The stale native helm-imenu cache was cleared before dispatch.
        (should-not alist-at-dispatch)
        (should-not candidates-at-dispatch)
        (should-not tick-at-dispatch)
        ;; The ast-grep one-shot cache written by helm-imenu is not left
        ;; behind for a later plain helm-imenu command.
        (dolist (cache '(helm-cached-imenu-alist
                         helm-cached-imenu-candidates
                         helm-cached-imenu-tick))
          (should-not (local-variable-p cache)))))))

(ert-deftest ast-grep-outline-test-helm-candidates-rebuild-after-cache-clear ()
  "Clearing helm-imenu's tick cache makes candidates rebuild."
  (skip-unless (require 'helm-imenu nil t))
  (with-temp-buffer
    (ast-grep-outline-test--insert-source)
    (setq buffer-file-name "x.ts")
    (setq-local imenu-create-index-function #'ast-grep--outline-imenu-index)
    (let ((calls 0)
          (old-candidates '(("old" . 1))))
      (setq-local helm-cached-imenu-tick (buffer-modified-tick))
      (setq-local helm-cached-imenu-candidates old-candidates)
      (cl-letf (((symbol-function 'ast-grep--run-outline)
                 (lambda (_file)
                   (setq calls (1+ calls))
                   ast-grep-outline-test--stream)))
        ;; The stale tick reproduces helm-imenu's short-circuit.
        (should (equal (helm-imenu-candidates (current-buffer))
                       old-candidates))
        (should (= calls 0))
        (ast-grep--outline-clear-helm-imenu-cache)
        (should-not (equal (helm-imenu-candidates (current-buffer))
                           old-candidates))
        (should (= calls 1))))))

(ert-deftest ast-grep-outline-test-command-helm-without-helm-uses-imenu ()
  "helm-mode active but Helm missing falls to imenu, never consult.
Runs in the consult sandbox, where consult is present but Helm is not."
  (skip-unless (and (require 'consult-imenu nil t)
                    (not (require 'helm-imenu nil t))))
  (with-temp-buffer
    (setq buffer-file-name "x.ts")
    (let ((ivy-mode nil)
          (helm-mode t)
          (called nil))
      (cl-letf (((symbol-function 'ast-grep--executable-available-p)
                 (lambda () t))
                ((symbol-function 'consult-imenu)
                 (lambda (&rest _) (interactive) (setq called 'consult)))
                ((symbol-function 'imenu)
                 (lambda (&rest _) (interactive) (setq called 'imenu))))
        (ast-grep-outline)
        (should (eq called 'imenu))))))

(provide 'ast-grep-outline-test)

;;; ast-grep-outline-test.el ends here
