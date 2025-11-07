# ast-grep.el Tests

This directory contains the test suite for ast-grep.el.

## Test Structure

### Unit Tests (`ast-grep-test.el`)
Basic unit tests that test individual functions in isolation, using mocks where necessary. These tests do not require `ast-grep` to be installed.

- Test utility functions
- Test command building
- Test JSON parsing
- Test error handling with mocks

### Integration Tests (`ast-grep-integration-test.el`)
Integration tests that require `ast-grep` to be installed and test the package with real ast-grep commands.

- Test real searches against fixture files
- Test command execution
- Test result parsing
- Test multiple search patterns

### Interactive Tests (`ast-grep-interactive-test.el`)
Tests that simulate user workflows and interactions with the package.

- Test navigation to search results
- Test buffer state management
- Test multiple sequential searches
- Test error handling in real scenarios

### Test Fixtures (`fixtures/`)
Sample code files used by integration and interactive tests:
- `sample.js` - JavaScript test file with functions and classes
- `sample.py` - Python test file with functions and classes

## Running Tests

### Using the test runner script (recommended)

```bash
# Run all tests
./run-tests.sh all

# Run only unit tests
./run-tests.sh unit

# Run only integration tests (requires ast-grep)
./run-tests.sh integration

# Run only interactive tests (requires ast-grep)
./run-tests.sh interactive

# Verbose mode
VERBOSE=1 ./run-tests.sh all
```

### Using Emacs directly

```bash
# Unit tests
emacs -batch -L . -l ast-grep.el -l tests/ast-grep-test.el -f ert-run-tests-batch-and-exit

# Integration tests
emacs -batch -L . -l ast-grep.el -l tests/ast-grep-integration-test.el -f ert-run-tests-batch-and-exit

# Interactive tests
emacs -batch -L . -l ast-grep.el -l tests/ast-grep-interactive-test.el -f ert-run-tests-batch-and-exit
```

### Running in Emacs interactively

1. Open Emacs
2. Load the test file: `M-x load-file RET tests/ast-grep-test.el RET`
3. Run tests: `M-x ert RET t RET`

## Requirements

- **All tests**: Emacs 28.1 or later
- **Integration & Interactive tests**: ast-grep CLI tool installed

Install ast-grep from: https://github.com/ast-grep/ast-grep

## CI Testing

Tests are automatically run in GitHub Actions on:
- Push to main branch
- Pull requests to main branch

The CI workflow tests against multiple Emacs versions:
- Emacs 28.2
- Emacs 29.4
- Emacs 30.1

CI also includes a test with Emacs daemon mode to ensure the package works in user-like interactive environments.

## Writing New Tests

When adding new tests:

1. **Unit tests**: Add to `ast-grep-test.el` for isolated function testing
2. **Integration tests**: Add to `ast-grep-integration-test.el` for tests requiring ast-grep
3. **Interactive tests**: Add to `ast-grep-interactive-test.el` for workflow simulation

Use descriptive test names following the pattern:
```elisp
(ert-deftest ast-grep-[category]-test-[description] ()
  "Test description."
  (should ...))
```

Mark tests that require ast-grep with:
```elisp
:expected-result (if (executable-find "ast-grep") :passed :failed)
(skip-unless (executable-find "ast-grep"))
```

## Test Coverage

The test suite covers:
- ✅ Executable availability checking
- ✅ Command building and execution
- ✅ JSON output parsing (both batch and streaming)
- ✅ Error handling
- ✅ Multiple search patterns
- ✅ Navigation to results
- ✅ Buffer state management
- ✅ Project root detection
- ✅ Mode activation
- ✅ Daemon mode compatibility

## Troubleshooting

### Tests fail with "ast-grep not found"
Install ast-grep: https://github.com/ast-grep/ast-grep

### Tests fail in batch mode but work interactively
Check that all required features are properly loaded with `require` statements.

### Integration tests fail with "file not found"
Ensure you're running tests from the repository root directory.
