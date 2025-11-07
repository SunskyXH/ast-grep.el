#!/bin/bash
# Test runner script for ast-grep.el
# Can be used locally or in CI environments

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
EMACS="${EMACS:-emacs}"
VERBOSE="${VERBOSE:-0}"
TEST_TYPE="${1:-all}"

echo "========================================"
echo "ast-grep.el Test Runner"
echo "========================================"
echo "Emacs: $EMACS"
echo "Test Type: $TEST_TYPE"
echo "========================================"

# Check if emacs is available
if ! command -v "$EMACS" &> /dev/null; then
    echo -e "${RED}Error: Emacs not found${NC}"
    echo "Please install Emacs or set the EMACS environment variable"
    exit 1
fi

# Check if ast-grep is available
if ! command -v ast-grep &> /dev/null; then
    echo -e "${YELLOW}Warning: ast-grep not found${NC}"
    echo "Integration tests will be skipped"
    echo "Install ast-grep from: https://github.com/ast-grep/ast-grep"
fi

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run a test suite
run_test_suite() {
    local test_name="$1"
    local test_file="$2"
    
    echo ""
    echo "----------------------------------------"
    echo -e "Running: ${GREEN}${test_name}${NC}"
    echo "----------------------------------------"
    
    if [ "$VERBOSE" = "1" ]; then
        "$EMACS" -batch -L . -l ast-grep.el -l "$test_file" -f ert-run-tests-batch-and-exit
    else
        "$EMACS" -batch -L . -l ast-grep.el -l "$test_file" -f ert-run-tests-batch-and-exit 2>&1
    fi
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ $test_name PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ $test_name FAILED${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# Run tests based on type
case "$TEST_TYPE" in
    unit)
        run_test_suite "Unit Tests" "tests/ast-grep-test.el"
        ;;
    integration)
        if command -v ast-grep &> /dev/null; then
            run_test_suite "Integration Tests" "tests/ast-grep-integration-test.el"
        else
            echo -e "${YELLOW}Skipping integration tests (ast-grep not found)${NC}"
        fi
        ;;
    interactive)
        if command -v ast-grep &> /dev/null; then
            run_test_suite "Interactive Tests" "tests/ast-grep-interactive-test.el"
        else
            echo -e "${YELLOW}Skipping interactive tests (ast-grep not found)${NC}"
        fi
        ;;
    all)
        run_test_suite "Unit Tests" "tests/ast-grep-test.el"
        
        if command -v ast-grep &> /dev/null; then
            run_test_suite "Integration Tests" "tests/ast-grep-integration-test.el"
            run_test_suite "Interactive Tests" "tests/ast-grep-interactive-test.el"
        else
            echo -e "${YELLOW}Skipping integration and interactive tests (ast-grep not found)${NC}"
        fi
        ;;
    *)
        echo -e "${RED}Error: Unknown test type: $TEST_TYPE${NC}"
        echo "Usage: $0 [unit|integration|interactive|all]"
        exit 1
        ;;
esac

# Summary
echo ""
echo "========================================"
echo "Test Summary"
echo "========================================"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"
echo "========================================"

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
