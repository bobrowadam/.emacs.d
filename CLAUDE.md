# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Configuration
- Main configuration file is init.org (2,869 lines)
- For this large config file, read only specific parts:
  - Use `rg "^\*+ "` to find all org headings first, then identify relevant sections
  - Use `rg` with specific patterns to find configuration blocks (e.g., `rg "use-package"`)
  - Use line numbers with Read tool to view specific sections after locating them
  - Search for package names or functionality with `rg` before reading those sections

## Core Files
- `init.org` - Master configuration file (2,869 lines) using literate programming
- `early-init.el` - Bootstrap configuration, GC optimization, Elpaca package manager setup
- `init.el` - Auto-generated from init.org via org-babel-load-file
- `modules/` directory contains modular configuration components

## Configuration Patterns
- Custom functions prefixed with `bob/` (33 functions total)
- Extensive AI/LLM integration (gptel, aider, minuet-ai, claude-code)
- Performance optimizations with GCMH and deferred loading
- Hebrew keyboard support integrated throughout
- Font selection based on monitor resolution

## Key Custom Functions
- `bob/read-file-content` - Utility for reading file contents
- `bob/eat-top-project` - Terminal in project root
- `bob/switch-to-open-project-buffer` - Project buffer switching
- `bob/kill-this-buffer` - Smart buffer killing

## Security
- Never commit secrets or API keys to the repository
- This is a personal Emacs configuration - avoid exposing sensitive customizations
- Be cautious when modifying authentication-related configurations
- Review any external package additions for security implications
- Use of encrypted auth sources for sensitive credentials

## Testing and Debugging

### Inspecting Elisp Code (Preferred)
Use elisp-dev MCP tools for inspecting code without side effects:
- `elisp-describe-function` - Get function documentation, signature, and check if function exists
- `elisp-get-function-definition` - Get function source code with file location
- `elisp-describe-variable` - Get variable info (without exposing values) and check if variable exists
- `elisp-info-lookup-symbol` - Look up symbols in Emacs documentation
- `elisp-read-source-file` - Read Elisp source files from Emacs directories

### Executing and Testing Code
Use `emacsclient --eval` when you need to execute code or modify state:
- Test functions: `emacsclient --eval "(bob/function-name args)"`
- Check syntax: `emacsclient --eval "(macroexpand 'expression)"`
- Reload config: `emacsclient --eval "(progn (compile-init-file) (load-file \"~/.emacs.d/init.el\"))"`
- Use this for operations that elisp-dev MCP doesn't support or when side effects are needed
