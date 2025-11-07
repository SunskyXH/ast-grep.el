# Contributing to ast-grep.el

Thank you for your interest in contributing to ast-grep.el!

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/SunskyXH/ast-grep.el.git
   cd ast-grep.el
   ```

2. Install dependencies:
   - Emacs 28.1 or later
   - [ast-grep](https://github.com/ast-grep/ast-grep) CLI tool

3. Run tests to verify setup:
   ```bash
   ./run-tests.sh all
   ```

## Making Changes

1. Create a new branch for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes following the coding style of the project.

3. Add or update tests for your changes (see Testing section below).

4. Run tests to ensure everything works:
   ```bash
   make test
   # or
   ./run-tests.sh all
   ```

5. Commit your changes with a descriptive message:
   ```bash
   git commit -m "Brief description of your changes"
   ```

6. Push to your fork and create a pull request.

## Testing

ast-grep.el has a comprehensive test suite to ensure quality and prevent regressions.

### Test Structure

- **Unit tests** (`tests/ast-grep-test.el`): Test individual functions in isolation
- **Integration tests** (`tests/ast-grep-integration-test.el`): Test with real ast-grep commands
- **Interactive tests** (`tests/ast-grep-interactive-test.el`): Simulate user workflows

### Running Tests

```bash
# Run all tests
make test
# or
./run-tests.sh all

# Run specific test suites
make test-unit
make test-integration
make test-interactive

# Run with verbose output
VERBOSE=1 ./run-tests.sh all
```

### Writing Tests

When adding new features:

1. **Add unit tests** for new functions:
   ```elisp
   (ert-deftest ast-grep-test-my-new-function ()
     "Test my new function."
     (should (equal (my-new-function "input") "expected-output")))
   ```

2. **Add integration tests** if the feature interacts with ast-grep:
   ```elisp
   (ert-deftest ast-grep-integration-test-my-feature ()
     "Integration test for my feature."
     :expected-result (if (executable-find "ast-grep") :passed :failed)
     (skip-unless (executable-find "ast-grep"))
     (should ...))
   ```

3. **Add interactive tests** for user-facing features:
   ```elisp
   (ert-deftest ast-grep-interactive-test-my-workflow ()
     "Test my interactive workflow."
     (with-temp-buffer
       ;; Simulate user interaction
       (should ...)))
   ```

### Test Fixtures

Add test fixtures in `tests/fixtures/` if you need sample files for testing.

## Code Style

- Follow standard Emacs Lisp conventions
- Use `lexical-binding: t`
- Add docstrings to all public functions
- Keep lines under 80 characters when possible
- Use meaningful variable and function names

## Documentation

Update documentation when:
- Adding new features
- Changing user-facing behavior
- Modifying configuration options

Documentation files:
- `README.org` - Main user documentation
- `tests/README.md` - Testing documentation
- Docstrings in code

## Continuous Integration

All pull requests are automatically tested using GitHub Actions with:
- Multiple Emacs versions (28.2, 29.4, 30.1)
- All test suites (unit, integration, interactive)
- Emacs daemon mode compatibility test

Ensure all tests pass before submitting your pull request.

## Getting Help

- Open an issue for bugs or feature requests
- Check existing issues and pull requests
- Read the code and tests for examples

## License

By contributing, you agree that your contributions will be licensed under the GPL-3.0 license.
