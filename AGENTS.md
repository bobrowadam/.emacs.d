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

## Git Workflow

### ⚠️ CRITICAL: Never Lose Changes!

**MOST IMPORTANT RULE**: When working with git commits, you must NEVER lose user changes. This is absolutely critical.

### Commit Guidelines

1. **Each commit should contain only related changes**
   - Group changes by logical functionality or purpose
   - Never mix unrelated changes in a single commit
   - Each commit should represent one coherent change

2. **When you need to separate changes that are already mixed:**
   
   **❌ NEVER DO THIS:**
   ```bash
   # This DESTROYS uncommitted changes!
   git checkout HEAD -- file.txt
   ```
   
   **✅ CORRECT APPROACH:**
   - Use `git stash` to safely preserve ALL changes before any git operations
   - Or create a temporary branch with all changes first
   - Use `git add -p` for interactive staging (but be aware of timeouts)
   - Or manually reapply specific changes using file editing tools
   
   **Example safe workflow:**
   ```bash
   # Save all changes first!
   git stash
   
   # Apply back and commit specific changes
   git stash pop
   # Now manually stage only related changes
   git add <specific-files>
   git commit -m "First logical change"
   
   # Remaining changes are still safely in working directory
   ```

3. **Before any destructive git operation:**
   - **ALWAYS** verify what changes will be lost
   - **ALWAYS** create a backup (stash, branch, or commit)
   - **NEVER** assume you can recreate changes from memory

4. **If you make a mistake:**
   - Use `git reflog` to find lost commits
   - Use `git stash list` to find stashed changes
   - Check if changes exist in previous commits with `git log --all -- <file>`

### Commit Message Format
- Use clear, descriptive commit messages
- Focus on the "why" not just the "what"
- Follow the existing style in `git log`

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
