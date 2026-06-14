;;; ast-grep-test.el --- Compatibility loader for ast-grep.el tests -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Compatibility entry point.  New tests live in focused files and
;; are loaded through tests/run-tests.el.

;;; Code:

(load (expand-file-name "run-tests.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(provide 'test-ast-grep)

;;; ast-grep-test.el ends here
