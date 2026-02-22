# Eglot TypeScript + Python Setup for bradwell-monorepo

**Date:** 2026-02-22
**Repo:** `~/source/bradwell-monorepo`

## Context

- Monorepo with TypeScript (NestJS services + React client) and Python (uv workspace)
- Biome for TS/JS linting and formatting (no prettier)
- Python services have no ruff/linter configured — pyright only
- Git worktrees used for feature work via superpowers skill

## What We're Changing

1. Install `rass` (rassumfrassum) — LSP multiplexer
2. Update TypeScript eglot server program in `init.org`
3. Remove noisy pyright auto-install block from `init.org`

## Design

### Install rass

```bash
pip3 install rassumfrassum
```

Installs the `rass` binary globally under `/opt/homebrew/bin/`.

### TypeScript: Dynamic rass + biome detection

Replace the current `("rass" "ts")` entry in `eglot-server-programs` with a contact function:

```elisp
(add-to-list 'eglot-server-programs
  `((js2-mode js-mode js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode)
    . ,(lambda (&rest _)
         (if-let* ((nm-dir (locate-dominating-file default-directory "node_modules"))
                   (biome  (expand-file-name "node_modules/.bin/biome" nm-dir))
                   ((file-executable-p biome)))
             (list "rass" "--" "typescript-language-server" "--stdio"
                   "--" biome "lsp-proxy")
           '("typescript-language-server" "--stdio")))))
```

**Why `locate-dominating-file`:** In a git worktree (e.g. `.claude/worktrees/feature-name/`),
`project-root` points to the worktree dir which has no `node_modules`. Walking upward finds
the main repo's `node_modules` correctly.

**Fallback:** Projects without biome get plain `typescript-language-server`.

### Python: No changes

Keep existing pyright config. Remove the `unless executable-find` auto-install block —
pyright is already installed and the async command on startup is noisy.

### Mason: No changes

`typescript-language-server` and `pyright-langserver` are already installed via fnm/npm.
Mason is available interactively (`M-x mason-install`) for future servers.

## Behavior Matrix

| Scenario | TypeScript | Python |
|----------|-----------|--------|
| bradwell-monorepo root | rass + ts-server + biome | pyright |
| bradwell-monorepo worktree | rass + ts-server + biome | pyright |
| Other project (no biome) | ts-server only | pyright |
