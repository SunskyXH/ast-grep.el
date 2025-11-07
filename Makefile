.POSIX:
.PHONY: test test-unit test-integration test-interactive clean help

EMACS ?= emacs
BATCH = $(EMACS) -batch -L . -l ast-grep.el

help:
	@echo "Available targets:"
	@echo "  test              - Run all tests"
	@echo "  test-unit         - Run unit tests only"
	@echo "  test-integration  - Run integration tests only"
	@echo "  test-interactive  - Run interactive tests only"
	@echo "  clean             - Remove generated files"
	@echo ""
	@echo "Environment variables:"
	@echo "  EMACS             - Emacs executable (default: emacs)"
	@echo "  VERBOSE           - Enable verbose output (default: 0)"

test:
	@./run-tests.sh all

test-unit:
	@./run-tests.sh unit

test-integration:
	@./run-tests.sh integration

test-interactive:
	@./run-tests.sh interactive

clean:
	find . -name '*.elc' -delete
	rm -f *~
	rm -f tests/*~

compile:
	$(BATCH) -f batch-byte-compile ast-grep.el

check-package:
	$(BATCH) \
	  --eval "(require 'package)" \
	  --eval "(package-initialize)" \
	  --eval "(package-install-file \"ast-grep.el\")"
