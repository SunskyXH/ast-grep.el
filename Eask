(package "ast-grep" "0.4.0"
         "Search code using ast-grep with completing-read interface")
(website-url "https://github.com/sunskyxh/ast-grep.el")
(keywords "tools" "matching")
(author "SunskyXH" "sunskyxh@gmail.com")
(license "GPL-3.0-or-later")

(package-file "ast-grep.el")
(depends-on "emacs" "28.1")

(source "gnu")
(source "nongnu")
(source "melpa")

(files "ast-grep*.el")

(script "compile"
        "eask --strict compile ast-grep-core.el ast-grep-sync.el ast-grep-consult.el ast-grep-ivy.el ast-grep.el")

(script "test"
        "eask emacs -batch -L . -L tests -l tests/run-tests.el -f ast-grep-test-run-batch")

(script "test:sync"
        "eask clean workspace && env AST_GREP_TEST_BACKEND=sync eask emacs -batch -L . -L tests -l tests/run-tests.el -f ast-grep-test-run-batch")

(script "test:consult"
        "eask clean workspace && eask install compat consult && env AST_GREP_TEST_BACKEND=consult eask emacs -batch -L . -L tests -l tests/run-tests.el -f ast-grep-test-run-batch")

(script "test:ivy"
        "eask clean workspace && eask install ivy counsel && env AST_GREP_TEST_BACKEND=ivy eask emacs -batch -L . -L tests -l tests/run-tests.el -f ast-grep-test-run-batch")

(script "test:full"
        "eask clean workspace && eask install compat consult ivy counsel && env AST_GREP_TEST_BACKEND=full eask emacs -batch -L . -L tests -l tests/run-tests.el -f ast-grep-test-run-batch")
