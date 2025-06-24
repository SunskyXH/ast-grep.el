# ast-grep.el

An Emacs interface to [ast-grep](https://github.com/ast-grep/ast-grep), a CLI tool for code structural search, lint and rewriting based on Abstract Syntax Tree patterns.

## Features

- Search code using ast-grep patterns with completing-read interface
- Project-wide search support
- Integration with completing-read frameworks (Vertico, etc.)


## Requirements

- Emacs 28.1 or later
- [ast-grep](https://github.com/ast-grep/ast-grep) CLI tool installed and available in PATH

## Usage

### Interactive Commands

- `ast-grep-search` - Search for patterns in current directory
- `ast-grep-project` - Search for patterns in current project  
- `ast-grep-directory` - Search for patterns in specified directory

### Minor Mode

Enable `ast-grep-mode` to show ast-grep status in the mode line.

## Configuration

Customize the following variables:

- `ast-grep-executable` - Path to ast-grep executable (default: "ast-grep")
- `ast-grep-arguments` - Default arguments passed to ast-grep (default: '("--json"))
- `ast-grep-debug` - Enable debug output for troubleshooting (default: nil)

